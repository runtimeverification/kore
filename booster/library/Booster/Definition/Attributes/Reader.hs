{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Parsing attributes from @ParsedAttributes@ to different internal
types. The required attribute names and parsers for the expected
values are hard-wired.
-}
module Booster.Definition.Attributes.Reader (
    HasAttributes (..),
    readLocation,
) where

import Control.Monad.Extra (whenM)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Char (isDigit)
import Data.Coerce (Coercible, coerce)
import Data.Kind
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as Text
import Text.Read (readEither)
import Text.Regex.PCRE

import Booster.Definition.Attributes.Base
import Booster.SMT.LowLevelCodec as SMT (parseSExpr)
import Booster.Syntax.ParsedKore.Base
import Booster.Util (Flag (..))
import Kore.Syntax.Json.Types (Id (..))

{- | A class describing all attributes we want to extract from parsed
 entities
-}
class HasAttributes ty where
    type Attributes ty :: Type

    mkAttributes :: ty -> Except Text (Attributes ty)

instance HasAttributes ParsedDefinition where
    type Attributes ParsedDefinition = DefinitionAttributes

    mkAttributes ParsedDefinition{attributes} =
        DefinitionAttributes
            <$> (maybe [] (map encodeUtf8) <$> attributes .:? "indexCells")

instance HasAttributes ParsedModule where
    type Attributes ParsedModule = ModuleAttributes

    mkAttributes _ = pure ModuleAttributes

instance HasAttributes ParsedAxiom where
    type Attributes ParsedAxiom = AxiomAttributes

    mkAttributes :: ParsedAxiom -> Except Text (Attributes ParsedAxiom)
    mkAttributes ParsedAxiom{attributes} =
        AxiomAttributes
            <$> readLocation attributes
            <*> readPriority attributes
            <*> attributes .:? "label"
            <*> (uniqueIdWithFallback <$> attributes .:? uniqueIdName <*> attributes .:? "label")
            <*> (attributes .! "simplification")
            <*> (attributes .! "preserves-definedness")
            <*> readConcreteness attributes
            <*> (attributes .! "smt-lemma")
            <*> readSyntacticClauses attributes
      where
        -- Some rewrite rules are dynamically generated and injected into
        -- a running server using the RPC "add-module" endpoint.
        -- These rules, for historical reasons, are not given unique ids. Hopefully they will be in future.
        -- Until then, we use the rule label, if available, in place of the ID.
        uniqueIdWithFallback :: Maybe Text -> Maybe Label -> UniqueId
        uniqueIdWithFallback mbUniqueId mbLabel = UniqueId $
            case mbUniqueId of
                Just uid -> uid
                Nothing -> fromMaybe "UNKNOWN" mbLabel

sourceName
    , locationName
    , uniqueIdName ::
        Text
sourceName = "org'Stop'kframework'Stop'attributes'Stop'Source"
locationName = "org'Stop'kframework'Stop'attributes'Stop'Location"
uniqueIdName = "UNIQUE'Unds'ID"

readLocation :: ParsedAttributes -> Except Text (Maybe Location)
readLocation attributes = do
    file <- attributes .:? sourceName
    case file of
        Nothing -> pure Nothing
        Just f -> Just . Location f <$> attributes .: locationName

{- | Reads 'priority' and 'simplification' attributes (with -optional-
   number in [0..200]) and 'owise' flag, returning either the given
   priority or lowest priority if 'owise'. Reports an error if more
   than one of these is present, defaults to 50 if none.
-}
readPriority :: ParsedAttributes -> Except Text Priority
readPriority attributes = do
    priority <-
        fmap ("priority",) <$> attributes .:? "priority"
    simplification <-
        fmap ("simplification",) <$> attributes .:? "simplification"
    owise <-
        fmap ("owise",) . (\b -> if b then Just maxBound else Nothing) <$> attributes .! "owise"
    case catMaybes [simplification, priority, owise] of
        [] ->
            pure 50
        [(_, p)] ->
            pure p
        more ->
            throwE $ "Several priorities given: " <> Text.intercalate "," (map fst more)

readConcreteness :: ParsedAttributes -> Except Text Concreteness
readConcreteness attributes = do
    concrete <- maybe (pure Nothing) ((Just <$>) . mapM readVar) $ getAttribute "concrete" attributes
    symbolic <- maybe (pure Nothing) ((Just <$>) . mapM readVar) $ getAttribute "symbolic" attributes
    case (concrete, symbolic) of
        (Nothing, Nothing) -> pure Unconstrained
        -- case concrete, symbolic(v1,...,vn)
        (Just [], Just _) -> do
            loc <- readLocation attributes
            throwE $ "Concreteness overlap at " <> Text.pack (show loc)
        -- case concrete(v1,...,vn), symbolic
        (Just _, Just []) -> do
            loc <- readLocation attributes
            throwE $ "Concreteness overlap at " <> Text.pack (show loc)
        (Just [], Nothing) -> pure $ AllConstrained Concrete
        (Nothing, Just []) -> pure $ AllConstrained Symbolic
        (cs', ss') ->
            let overlap = Set.fromList (fromMaybe [] cs') `Set.intersection` Set.fromList (fromMaybe [] ss')
             in if not $ null overlap
                    then do
                        loc <- readLocation attributes
                        throwE $
                            "Concreteness overlap for "
                                <> Text.intercalate "," (Set.toList $ Set.map (Text.decodeUtf8 . fst) overlap)
                                <> " at "
                                <> Text.pack (show loc)
                    else
                        pure $
                            SomeConstrained $
                                Map.fromList [(c, Concrete) | c <- fromMaybe [] cs']
                                    <> Map.fromList [(s, Symbolic) | s <- fromMaybe [] ss']
  where
    readVar str = except $ case Text.splitOn ":" str of
        [name, sort] -> Right (Text.encodeUtf8 name, Text.encodeUtf8 sort)
        _ -> Left "Invalid variable"

{- | Reads 'syntactic' attribute, returning the set of integer indices.
     Reports an error if any of the integers are non-positive or there are duplicates.
     Defaults to an empty set.
-}
readSyntacticClauses :: ParsedAttributes -> Except Text SyntacticClauses
readSyntacticClauses attributes = do
    syntacticClauses <-
        maybe (pure Nothing) ((Just <$>) . mapM (except . readWord8)) $
            getAttribute "syntactic" attributes
    case syntacticClauses of
        Nothing -> pure $ SyntacticClauses []
        Just [] -> pure $ SyntacticClauses []
        Just more -> pure $ SyntacticClauses more
  where
    readWord8 str
        | all isDigit (Text.unpack str) = first Text.pack $ readEither (Text.unpack str)
        | otherwise = Left $ "invalid syntactic clause" <> (Text.pack $ show str)

instance HasAttributes ParsedSymbol where
    type Attributes ParsedSymbol = SymbolAttributes

    mkAttributes ParsedSymbol{name, attributes} = do
        isInjctn <- attributes .! "sortInjection"
        let symbolType = do
                isConstr <- attributes .! "constructor"
                isFunctn <- attributes .! "function"
                isTotal <- attributes .! "total" <||> attributes .! "functional"
                case (isConstr, isInjctn, isFunctn, isTotal) of
                    (True, _, _, _) -> pure Constructor
                    (_, True, _, _) -> pure Constructor
                    (_, _, _, True) -> pure $ Function Total
                    (_, _, True, _) -> pure $ Function Partial
                    _other ->
                        throwE $ "Invalid symbol type '" <> name.getId <> "', attributes: " <> Text.pack (show attributes)
            isIdem = do
                whenM (attributes .! "sortInjection" <&&> attributes .! "idem") $
                    throwE $
                        "Sort injection '" <> name.getId <> "' cannot be idempotent."
                attributes .! "idem"
            isAssoc = do
                whenM (attributes .! "sortInjection" <&&> attributes .! "assoc") $
                    throwE $
                        "Sort injection '" <> name.getId <> "' cannot be associative."
                attributes .! "assoc"
            hasConcreteEvaluators = coerce . not <$> attributes .! "no-evaluators"
            smt = do
                hooked <- attributes .:? "smt-hook"
                declared <- attributes .:? "smtlib"
                case (hooked, declared) of
                    (Just h, _) ->
                        either (throwE . Text.pack) (pure . Just . SMTHook) $
                            SMT.parseSExpr (encodeUtf8 h)
                    (Nothing, Just txt) ->
                        pure . Just . SMTLib $ encodeUtf8 txt
                    (Nothing, Nothing) ->
                        pure Nothing
            hook = do
                hooked <- attributes .:? "hook"
                pure (encodeUtf8 <$> hooked)

        SymbolAttributes
            <$> symbolType
            <*> isIdem
            <*> isAssoc
            <*> (coerce <$> (attributes .! "macro" <||> attributes .! "alias'Kywd'"))
            <*> hasConcreteEvaluators
            <*> pure Nothing
            <*> smt
            <*> hook

instance HasAttributes ParsedSort where
    type Attributes ParsedSort = SortAttributes

    mkAttributes ParsedSort{sortVars, attributes} = do
        mElem <- attributes .:? "element"
        mConcat <- attributes .:? "concat"
        mUnit <- attributes .:? "unit"
        hook <- attributes .:? "hook"
        case (encodeUtf8 <$> mElem, encodeUtf8 <$> mConcat, encodeUtf8 <$> mUnit, hook) of
            (Just elementSymbolName, Just concatSymbolName, Just unitSymbolName, Just ("MAP.Map" :: Text)) ->
                pure
                    SortAttributes
                        { argCount = length sortVars
                        , collectionAttributes =
                            Just
                                ( KCollectionSymbolNames
                                    { unitSymbolName
                                    , elementSymbolName
                                    , concatSymbolName
                                    }
                                , KMapTag
                                )
                        }
            (Just elementSymbolName, Just concatSymbolName, Just unitSymbolName, Just ("LIST.List" :: Text)) ->
                pure
                    SortAttributes
                        { argCount = length sortVars
                        , collectionAttributes =
                            Just
                                ( KCollectionSymbolNames
                                    { unitSymbolName
                                    , elementSymbolName
                                    , concatSymbolName
                                    }
                                , KListTag
                                )
                        }
            (Just elementSymbolName, Just concatSymbolName, Just unitSymbolName, Just ("SET.Set" :: Text)) ->
                pure
                    SortAttributes
                        { argCount = length sortVars
                        , collectionAttributes =
                            Just
                                ( KCollectionSymbolNames
                                    { unitSymbolName
                                    , elementSymbolName
                                    , concatSymbolName
                                    }
                                , KSetTag
                                )
                        }
            (Just _, Just _, Just _, Just _) ->
                pure
                    SortAttributes
                        { argCount = length sortVars
                        , collectionAttributes = Nothing
                        }
            (Nothing, Nothing, Nothing, _) ->
                pure
                    SortAttributes
                        { argCount = length sortVars
                        , collectionAttributes = Nothing
                        }
            _ -> throwE "Malformed hooked sort. Should contain unit, element and concat."

----------------------------------------

readError :: Text -> String -> Text
readError name msg = name <> " could not be read: " <> Text.pack msg

extractAttribute :: ReadT a => Text -> ParsedAttributes -> Except Text a
extractAttribute name attribs =
    (maybe (throwE notFound) pure $ getAttribute name attribs)
        >>= except . first (readError name) . readT
  where
    notFound = name <> " not found in attributes."

(.:) :: ReadT a => ParsedAttributes -> Text -> Except Text a
(.:) = flip extractAttribute

(.:?) :: forall a. ReadT a => ParsedAttributes -> Text -> Except Text (Maybe a)
attribs .:? name =
    except . first (readError name) . mapM readT $ getAttribute name attribs

(.!) :: Coercible Bool b => ParsedAttributes -> Text -> Except Text b
attrs .! name = except $ case getAttribute name attrs of
    Nothing -> Right $ coerce False
    Just _ -> Right $ coerce True

infix 5 .!
infix 5 .:?
infix 5 .:

-- see GHC.Utils.Misc:
(<&&>), (<||>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)
(<||>) = liftA2 (||)
infixr 3 <&&>
infixr 2 <||>

----------------------------------------

-- | Type class providing safe readers for different types
class ReadT a where
    readT :: [Text] -> Either String a

instance ReadT Priority where
    readT [] = Right 50 -- HACK to accept `simplification()` from internal modules
    readT [n]
        | Text.null n = Right 50 -- HACK to accept `simplification("")`
        | all isDigit (Text.unpack n) = Priority <$> readEither (Text.unpack n)
        | otherwise = Left $ "invalid priority value " <> show n
    readT ns = Left $ "invalid priority value " <> show ns

instance ReadT Text where
    readT :: [Text] -> Either String Text
    readT [] = Left "empty"
    readT [t] = Right t
    readT ts = Left $ "invalid text value " <> show ts

-- read lists by reading each part as a singleton list
instance ReadT a => ReadT [a] where
    readT = mapM (readT . (: []))

instance ReadT Position where
    readT [] = Left "empty position"
    readT [p] = readLocationType p
      where
        readLocationType :: Text -> Either String Position
        readLocationType input =
            case input %%~ locRegex of
                ("", _match, "", [lineStr, columnStr, _, _]) ->
                    Right $ Position (read lineStr) (read columnStr)
                _other ->
                    Left $ show input <> ": garbled location data"

        natRegex, locRegex :: String
        natRegex = "(0|[1-9][0-9]*)"
        locRegex =
            mconcat
                [ "^Location\\("
                , " *"
                , natRegex
                , ","
                , " *"
                , natRegex
                , ","
                , " *"
                , natRegex
                , ","
                , " *"
                , natRegex
                , "\\)$"
                ]
    readT ps = Left $ "invalid position value " <> show ps

-- Strips away the Source(...) constructor that gets printed, if there
-- is one. If there is none, it uses the attribute string as-is.
instance ReadT FileSource where
    readT [] = Left "empty file source"
    readT [f] = readSource f
      where
        readSource :: Text -> Either String FileSource
        readSource input =
            case input %%~ "^Source\\((..*)\\)$" of
                ("", _all, "", [file]) ->
                    Right $ FileSource file
                (unmatched, "", "", []) ->
                    Right $ FileSource unmatched
                _other ->
                    Left $ "bad source: " <> show input
    readT fs = Left $ "invalid source value " <> show fs

-- helper to pin regex match type
(%%~) :: Text -> String -> (String, String, String, [String])
txt %%~ regex = Text.unpack txt =~ regex
