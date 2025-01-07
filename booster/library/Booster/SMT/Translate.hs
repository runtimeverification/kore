{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Translate (
    TranslationState (..),
    Translator (..),
    equationToSMTLemma,
    initTranslator,
    smtDeclarations,
    translateTerm,
    valueToTerm,
    backTranslateFrom,
    runTranslator,
    smtSort,
    selectLemmas,
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Prettyprinter (pretty)
import Prettyprinter qualified as Pretty
import Text.Read (readMaybe)

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Pattern.Pretty
import Booster.Pattern.Util (filterTermSymbols, isFunctionSymbol, sortOfTerm)
import Booster.Prettyprinter qualified as Pretty
import Booster.SMT.Base as SMT
import Booster.SMT.LowLevelCodec as SMT
import Booster.Util (Flag (..))

data TranslationState = TranslationState
    { mappings :: Map Term SMTId
    , counter :: !Int
    }

initTranslator :: TranslationState
initTranslator =
    TranslationState{mappings = mempty, counter = 1}

newtype Translator a = Translator (StateT TranslationState (Except Text) a)
    deriving newtype (Functor, Applicative, Monad)

throw :: Text -> Translator a
throw = Translator . lift . throwE

runTranslator :: Translator a -> Either Text (a, TranslationState)
runTranslator (Translator action) = runExcept $ runStateT action initTranslator

asSMTVar :: Term -> Translator SExpr
asSMTVar t = Translator $ do
    st <- get
    case Map.lookup t st.mappings of
        Just v -> pure $ Atom v
        Nothing -> do
            let new = SMTId . BS.pack $ "SMT-" <> show st.counter
            put
                st
                    { mappings = Map.insert t new st.mappings
                    , counter = st.counter + 1
                    }
            pure $ Atom new

translateTerm :: Term -> Translator SExpr
translateTerm t =
    case t of
        AndTerm t1 t2
            | SortBool <- sortOfTerm t1
            , SortBool <- sortOfTerm t2 ->
                SMT.and <$> translateTerm t1 <*> translateTerm t2
            | otherwise ->
                throw $
                    "General \and not supported for SMT. Failed to translate "
                        <> Pretty.renderText (pretty (PrettyWithModifiers @['Decoded, 'Truncated] t))
        SymbolApplication sym _sorts args ->
            case sym.attributes.smt of
                Nothing -> asSMTVar t
                Just (SMTLib name)
                    | null args -> pure (Atom (SMTId name))
                    | otherwise -> do
                        smtArgs <- mapM translateTerm args
                        pure . List $ Atom (SMTId name) : smtArgs
                Just (SMTHook hook@Atom{})
                    | null args -> pure hook
                    | otherwise -> do
                        smtArgs <- mapM translateTerm args
                        pure . List $ hook : smtArgs
                Just (SMTHook sexpr) -> do
                    smtArgs <- mapM translateTerm args
                    fillPlaceholders sexpr smtArgs
        DomainValue sort value
            | SortBool <- sort ->
                pure $ Atom (SMTId value)
            | SortInt <- sort ->
                pure $ Atom (SMTId value)
            | otherwise ->
                asSMTVar t
        _other ->
            asSMTVar t

-- Atoms of the shape "#<num>" where <num> is a small positive
-- integer are replaced with the element at index <num>.
fillPlaceholders :: SExpr -> [SExpr] -> Translator SExpr
fillPlaceholders target list = go target
  where
    go :: SExpr -> Translator SExpr
    go (Atom symb) = fillAtom symb
    go (List sexprs) = List <$> mapM go sexprs

    maxArg = length list

    fillAtom :: SMTId -> Translator SExpr
    fillAtom name@(SMTId bs)
        | '#' == BS.head bs
        , BS.length bs > 1
        , Just n <- readMaybe @Int (BS.unpack $ BS.tail bs) =
            if n > maxArg
                then throw $ "Hook argument index out of bounds: " <> pack (show target)
                else pure $ list !! (n - 1)
        | otherwise = pure $ Atom name

valueToTerm :: TranslationState -> Value -> Either Text Term
valueToTerm st = \case
    Bool True -> Right TrueBool
    Bool False -> Right FalseBool
    Int i -> Right $ DomainValue SortInt (BS.pack $ show i)
    Other sexpr -> backTranslateFrom st sexpr

backTranslateFrom :: TranslationState -> SExpr -> Either Text Term
backTranslateFrom st = \case
    Atom s@(SMTId v)
        | isVar v ->
            first (pack (show v) <>) $
                fromMaybe (Left "Not found in reverseMap") $
                    Map.lookup s reverseMap
        | v == "true" ->
            Right TrueBool
        | v == "false" ->
            Right FalseBool
        | BS.all isDigit v ->
            Right $ DomainValue SortInt v
        | otherwise ->
            Left $ "Unable to backtranslate atom " <> pack (show v)
    List _ -> Left "backtranslate: List case not implemented"
  where
    reverseMap :: Map SMTId (Either Text Term)
    reverseMap =
        Map.map (first $ ("Duplicate bindings in reverseMap: " <>) . pack . show)
            . Map.fromListWith collectDuplicates
            . map (\(a, b) -> (b, Right a))
            $ Map.assocs st.mappings

    collectDuplicates (Right x) (Right y) = Left [x, y]
    collectDuplicates (Left xs) (Right y) = Left $ y : xs
    collectDuplicates (Right x) (Left ys) = Left $ x : ys
    collectDuplicates (Left xs) (Left ys) = Left $ xs <> ys

    isVar :: BS.ByteString -> Bool
    isVar bs = first4 == "SMT-" && BS.all isDigit rest
      where
        (first4, rest) = BS.splitAt 4 bs

-- render an SMT assertion from an SMT lemma (which exist for both
-- kinds of equations,"Function" and "Simplification")
equationToSMTLemma :: RewriteRule a -> Translator (Maybe DeclareCommand)
equationToSMTLemma equation
    | not (coerce equation.attributes.smtLemma) = pure Nothing
    | otherwise = fmap Just $ do
        smtLHS <- translateTerm equation.lhs
        smtRHS <- translateTerm equation.rhs
        let equationRaw = SMT.eq smtLHS smtRHS
        -- if requires empty: just (= (lhs) (rhs))
        -- if requires not empty: (=> (requires) (= (lhs) (rhs)))
        lemmaRaw <-
            if null equation.requires
                then pure equationRaw
                else do
                    smtPremise <-
                        foldl1 SMT.and
                            <$> mapM (translateTerm . \(Predicate t) -> t) equation.requires
                    pure $ SMT.implies smtPremise equationRaw
        -- NB ensures has no SMT implications (single shot sat-check)

        finalMapping <- Translator $ gets (.mappings)
        -- for detailed error messages:
        let prettyMappings m =
                Pretty.vsep
                    [ Pretty.pretty (show v) <> " <== " <> Pretty.pretty (PrettyWithModifiers @['Decoded, 'Truncated] t)
                    | (t, v) <- Map.toList m
                    ]
            lemmaId =
                head $
                    catMaybes
                        [ fmap Pretty.pretty equation.attributes.ruleLabel
                        , fmap Pretty.pretty equation.attributes.location
                        , Just "Unknown location"
                        ]
        -- free variables (not created by abstraction during
        -- translation) are all-quantified on the outside
        let freeVars = Set.toList (getAttributes equation.lhs).variables
            -- LHS should be ok since RHS of an equation should not have existentials.
            mkSExpPair :: Variable -> Translator SExpr
            mkSExpPair v
                | Just smtV <- Map.lookup (Var v) finalMapping =
                    pure $ List [Atom smtV, SMT.sortExpr $ smtSort v.variableSort]
                | otherwise =
                    throw . Pretty.renderText $
                        Pretty.vsep
                            [ "Free variable " <> pretty (show v.variableName) <> " not found in "
                            , prettyMappings finalMapping
                            ]
        varPairs <- mapM mkSExpPair freeVars
        -- An SMT lemma should not contain any uninterpreted
        -- functions. If anything was variable-abstracted apart from
        -- the free variables in the term, this is an error.
        let surplusMappings = foldr (Map.delete . Var) finalMapping freeVars
        unless (Map.null surplusMappings) $ do
            throw . Pretty.renderText $
                Pretty.vsep
                    [ "Surplus mappings found for lemma " <> lemmaId
                    , prettyMappings surplusMappings
                    ]
        -- reset state but keep variable counter
        Translator . modify $ \s -> s{mappings = Map.empty}
        let varComments =
                [ smtV <> " <-- " <> v.variableName
                | (Var v, SMTId smtV) <- Map.assocs finalMapping
                ]
            prettyLemma =
                Pretty.vsep
                    ( pretty (PrettyWithModifiers @['Decoded, 'Truncated] equation.lhs)
                        <> " == "
                        <> pretty (PrettyWithModifiers @['Decoded, 'Truncated] equation.rhs)
                        : if null equation.requires
                            then []
                            else
                                "  requires"
                                    : map
                                        ((Pretty.indent 4 . pretty) . (PrettyWithModifiers @['Decoded, 'Truncated]))
                                        equation.requires
                    )
            lemmaComment = BS.pack (Pretty.renderDefault prettyLemma)

        pure . Assert (BS.unlines $ lemmaComment : varComments) $
            List [Atom "forall", List varPairs, lemmaRaw]

-- collect and render all declarations from a definition
smtDeclarations ::
    KoreDefinition -> Either Text ([DeclareCommand], Map SymbolName (Set DeclareCommand))
smtDeclarations def
    | Left msg <- translatedLemmas =
        Left $ "Lemma translation failed: " <> msg
    | Right (_, finalState) <- translatedLemmas
    , not (Map.null finalState.mappings) =
        Left . pack $ "Unexpected final state " <> show (finalState.mappings, finalState.counter)
    | Right (lemmas, _) <- translatedLemmas =
        Right (sortDecls <> funDecls, lemmas)
  where
    -- declare all sorts except Int and Bool
    sortDecls =
        [ DeclareSort ("User-defined sort " <> name) (quoted name) attributes.argCount
        | (name, (attributes, _)) <- Map.assocs def.sorts
        , name /= "SortInt"
        , name /= "SortBool"
        ]
    -- declare all functions that have smt-lib
    funDecls =
        mapMaybe declareFunc $ Map.elems def.symbols

    -- declare all SMT lemmas as assertions and construct a lookup table Symbol -> Lemmas
    allSMTEquations :: Theory (RewriteRule t) -> Translator [(RewriteRule t, DeclareCommand)]
    allSMTEquations =
        fmap catMaybes
            . mapM (\e -> fmap (e,) <$> equationToSMTLemma e)
            . filter (coerce . (.attributes.smtLemma))
            . concat
            . concatMap Map.elems
            . Map.elems

    -- collect function symbols of an equation (LHS + requires, RHS)
    collectSymbols :: RewriteRule t -> ([SymbolName], [SymbolName])
    collectSymbols rule =
        ( collectNames rule.lhs <> concatMap (collectNames . coerce) rule.requires
        , collectNames rule.rhs
        )

    -- symbol used on LHS => lookup must include sym -> this rule
    -- symbol used on RHS => lookups returning this rule must be
    -- _extended_ by rules reachable from that symbol. Requires a
    -- transitive closure of the lookup map.

    initialLookup ::
        Theory (RewriteRule t) ->
        Translator (Map SymbolName (Set (RewriteRule t, DeclareCommand)))
    initialLookup = fmap (Map.unionsWith (<>) . map mapFrom) . allSMTEquations
      where
        mapFrom (eqn, smt) =
            Map.fromList [(sym, Set.singleton (eqn, smt)) | sym <- fst $ collectSymbols eqn]

    closeOverSymbols ::
        forall a t.
        Ord a =>
        Map SymbolName (Set (RewriteRule t, a)) ->
        Map SymbolName (Set (RewriteRule t, a))
    closeOverSymbols start = go start
      where
        keys = Map.keys start -- should not change
        go ::
            Map SymbolName (Set (RewriteRule t, a)) -> Map SymbolName (Set (RewriteRule t, a))
        go current =
            let new = execState (mapM updateMapFor keys) current
             in if new == current then new else go new

        updateMapFor ::
            SymbolName -> State (Map SymbolName (Set (RewriteRule t, a))) ()
        updateMapFor k = do
            m <- get
            case Map.lookup k m of
                Nothing -> pure () -- should not happen, keys won't change
                Just eqs -> do
                    let rhsSyms = concatMap (snd . collectSymbols . fst) $ toList eqs
                        newEqs = Set.unions $ mapMaybe (flip Map.lookup m) rhsSyms
                        newM = Map.update (Just . (<> newEqs)) k m
                    put newM

    translatedLemmas :: Either Text (Map SymbolName (Set DeclareCommand), TranslationState)
    translatedLemmas =
        let trans :: Theory (RewriteRule t) -> Translator (Map SymbolName (Set DeclareCommand))
            trans = fmap (Map.map (Set.map snd) . closeOverSymbols) . initialLookup
         in runTranslator $
                (<>) <$> trans def.simplifications <*> trans def.functionEquations

    -- kore-rpc also declares all constructors, with no-junk axioms. WHY?

    declareFunc :: Symbol -> Maybe DeclareCommand
    declareFunc sym
        | Just (SMTLib name) <- sym.attributes.smt =
            Just $
                DeclareFunc
                    ("smtlib-attributed function " <> sym.name)
                    (smtName name)
                    (map smtSort sym.argSorts)
                    (smtSort sym.resultSort)
        | otherwise = Nothing

-- | helper to select SMT lemmas from the context given a predicate to check
selectLemmas :: Map SymbolName (Set DeclareCommand) -> [Predicate] -> [DeclareCommand]
selectLemmas m ps =
    Set.toList $ Set.unions $ mapMaybe (flip Map.lookup m) usedFcts
  where
    usedFcts = concatMap (collectNames . coerce) ps

collectNames :: Term -> [SymbolName]
collectNames = map (.name) . filterTermSymbols isFunctionSymbol

smtName, quoted :: BS.ByteString -> SMTId
smtName = SMTId
-- All Kore sort names (except Int and Bool) need to be quoted |...| here.
quoted bs = SMTId $ "|" <> bs <> "|"

smtSort :: Sort -> SMTSort
smtSort SortInt = SimpleSMTSort "Int"
smtSort SortBool = SimpleSMTSort "Bool"
smtSort (SortApp sortName args)
    | null args = SimpleSMTSort $ quoted sortName
    | otherwise = SMTSort (quoted sortName) $ map smtSort args
smtSort (SortVar varName) =
    error $ "Sort variable " <> show varName <> " not supported for SMT"
