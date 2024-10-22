{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Util (
    substituteInSort,
    sortOfTerm,
    sortOfPattern,
    modifyVariables,
    modifyVariablesInT,
    modifyVariablesInP,
    modifyVarName,
    modifyVarNameConcreteness,
    freeVariables,
    isConstructorSymbol,
    isFunctionSymbol,
    isDefinedSymbol,
    checkSymbolIsAc,
    checkTermSymbols,
    isConcrete,
    filterTermSymbols,
    sizeOfTerm,
    termVarStats,
    termSymbolStats,
    freshenVar,
    abstractTerm,
    abstractSymbolicConstructorArguments,
    cellSymbolStats,
    cellVariableStats,
    stripMarker,
    markAsExVar,
    markAsRuleVar,
    incrementNameCounter,
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 qualified as A
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Functor.Foldable (Corecursive (embed), cata)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Booster.Definition.Attributes.Base (
    Concreteness (..),
    FunctionType (..),
    KCollectionMetadata (..),
    KListDefinition (..),
    KMapDefinition (..),
    SymbolAttributes (..),
    SymbolType (..),
 )
import Booster.Pattern.Base
import Booster.Util (Flag (..))

-- | Returns the sort of a term
sortOfTerm :: Term -> Sort
sortOfTerm (AndTerm _ child) = sortOfTerm child
sortOfTerm (SymbolApplication symbol sorts _) =
    substituteInSort (Map.fromList $ zip symbol.sortVars sorts) symbol.resultSort
sortOfTerm (DomainValue sort _) = sort
sortOfTerm (Var Variable{variableSort}) = variableSort
sortOfTerm (Injection _ sort _) = sort
sortOfTerm (KMap def _ _) = SortApp def.mapSortName []
sortOfTerm (KList def _ _) = SortApp def.listSortName []
sortOfTerm (KSet def _ _) = SortApp def.listSortName []

substituteInSort :: Map VarName Sort -> Sort -> Sort
substituteInSort subst var@(SortVar n) =
    fromMaybe var $ Map.lookup n subst
substituteInSort subst (SortApp n args) =
    SortApp n $ map (substituteInSort subst) args

sortOfPattern :: Pattern -> Sort
sortOfPattern pat = sortOfTerm pat.term

modifyVariables :: (Variable -> Variable) -> Pattern -> Pattern
modifyVariables f p =
    Pattern
        { term = modifyVariablesInT f p.term
        , constraints = Set.map (modifyVariablesInP f) p.constraints
        , substitution = Map.map (coerce $ modifyVariablesInT f) p.substitution
        , ceilConditions = map (coerce $ modifyVariablesInT f) p.ceilConditions
        }

modifyVariablesInT :: (Variable -> Variable) -> Term -> Term
modifyVariablesInT f = cata $ \case
    VarF v -> Var (f v)
    other -> embed other

modifyVariablesInP :: (Variable -> Variable) -> Predicate -> Predicate
modifyVariablesInP = coerce modifyVariablesInT

modifyVarName :: (VarName -> VarName) -> Variable -> Variable
modifyVarName f v = v{variableName = f v.variableName}

markAsRuleVar :: VarName -> VarName
markAsRuleVar = ("Rule#" <>)

markAsExVar :: VarName -> VarName
markAsExVar = ("Ex#" <>)

{- | Strip variable provenance prefixes introduced using "markAsRuleVar" and "markAsExVar"
in "Syntax.ParsedKore.Internalize"
-}
stripMarker :: VarName -> VarName
stripMarker name =
    let noRule = BS.stripPrefix "Rule#" name
        noEx = BS.stripPrefix "Ex#" name
     in fromMaybe name $ noRule <|> noEx

freshenVar :: Variable -> Set Variable -> Variable
freshenVar v@Variable{variableName = vn} vs
    | v `Set.member` vs = freshenVar v{variableName = incrementNameCounter vn} vs
    | otherwise = v

incrementNameCounter :: VarName -> VarName
incrementNameCounter vname =
    let (name, counter) = BS.spanEnd A.isDigit_w8 vname
        parsedCounter = A.parseOnly @Int A.decimal counter
        newCounter = case parsedCounter of
            Right ok -> ok + 1
            Left _err -> 0
        -- convert the incremented counter back into a bytestring
        unparsedNewCounter = BS.pack . map (fromIntegral . ord) . show $ newCounter
     in name <> unparsedNewCounter

{- | Abstract a term into a variable, making sure the variable name is disjoint from the given set of variables.
     Return the resulting singleton substitution.
-}
abstractTerm :: Set Variable -> Term -> (Term, Term)
abstractTerm vs term =
    let
        varName = BS.pack . map (fromIntegral . ord) $ "HSVAR" <> show (abs (getAttributes term).hash)
        var = Variable (sortOfTerm term) varName
     in
        (Var (freshenVar var vs), term)

{- | Abstract the _arguments_ of given symbols in a term, making sure that they symbols are constructors.
     Return the original term if abstraction failed.

     Example (KEVM):
       abstractSymbolicConstructorArguments (<kevm> ... <gas> LARGE_EXPR </gas> ... </kevm> ['<gas>'])
       should turn LARGE_EXPR into a fresh variable
-}
abstractSymbolicConstructorArguments :: Set SymbolName -> Term -> Term
abstractSymbolicConstructorArguments constructorNames term = goSubst (freeVariables term) term
  where
    goSubst freeVars t = case t of
        Var{} -> t
        DomainValue{} -> t
        SymbolApplication sym sorts args ->
            if sym.attributes.symbolType == Constructor && sym.name `Set.member` constructorNames
                then
                    SymbolApplication sym sorts $
                        map (\x -> if not (isConcrete x) then fst . abstractTerm freeVars $ x else x) args
                else SymbolApplication sym sorts $ map (goSubst freeVars) args
        AndTerm t1 t2 -> AndTerm (goSubst freeVars t1) (goSubst freeVars t2)
        Injection ss s sub -> Injection ss s (goSubst freeVars sub)
        KMap attrs keyVals rest ->
            KMap attrs (bimap (goSubst freeVars) (goSubst freeVars) <$> keyVals) (goSubst freeVars <$> rest)
        KList def heads rest ->
            KList
                def
                (map (goSubst freeVars) heads)
                (bimap (goSubst freeVars) (map (goSubst freeVars)) <$> rest)
        KSet def elements rest ->
            KSet def (map (goSubst freeVars) elements) (goSubst freeVars <$> rest)

modifyVarNameConcreteness :: (ByteString -> ByteString) -> Concreteness -> Concreteness
modifyVarNameConcreteness f = \case
    SomeConstrained m -> SomeConstrained $ Map.mapKeys (first f) m
    other -> other

freeVariables :: Term -> Set Variable
freeVariables (Term attributes _) = attributes.variables

isConcrete :: Term -> Bool
isConcrete = Set.null . freeVariables

isConstructorSymbol :: Symbol -> Bool
isConstructorSymbol symbol =
    case symbol.attributes.symbolType of
        Constructor -> True
        _ -> False

isFunctionSymbol :: Symbol -> Bool
isFunctionSymbol symbol =
    case symbol.attributes.symbolType of
        Function _ -> True
        _ -> False

isDefinedSymbol :: Symbol -> Bool
isDefinedSymbol symbol =
    case symbol.attributes.symbolType of
        Constructor -> True
        Function Total -> True
        Function Partial -> False

checkSymbolIsAc :: Symbol -> Bool
checkSymbolIsAc symbol =
    coerce symbol.attributes.isAssoc || coerce symbol.attributes.isIdem

checkTermSymbols :: (Symbol -> Bool) -> Term -> Bool
checkTermSymbols check = cata $ \case
    SymbolApplicationF symbol _ ts -> check symbol && and ts
    other -> and other

filterTermSymbols :: (Symbol -> Bool) -> Term -> [Symbol]
filterTermSymbols check = cata $ \case
    SymbolApplicationF symbol _ ts
        | check symbol -> symbol : concat ts
        | otherwise -> concat ts
    AndTermF t1 t2 -> t1 <> t2
    DomainValueF _ _ -> []
    VarF _ -> []
    InjectionF _ _ t -> t
    -- Note the different cases:
    -- - empty collection: unit symbol only
    -- - singleton collections: element symbol only
    -- - opaque only: no collection symbols required
    -- - non-empty collection: no unit symbol required
    KMapF def [] Nothing ->
        [unitSym | let unitSym = unitSymbol $ KMapMeta def, check unitSym]
    KMapF def [(k, v)] Nothing ->
        k <> v <> [elemSym | let elemSym = kmapElementSymbol def, check elemSym]
    KMapF _ [] (Just t) ->
        t
    KMapF def kvs t ->
        let
            concatSym = concatSymbol $ KMapMeta def
            elementSym = kmapElementSymbol def
         in
            filter check [concatSym, elementSym]
                <> concatMap (uncurry (<>)) kvs
                <> fromMaybe [] t
    KListF def heads Nothing ->
        let concatSym = concatSymbol $ KListMeta def
            elemSym = klistElementSymbol def
            unitSym = unitSymbol $ KListMeta def
         in case heads of
                [] -> [unitSym | check unitSym]
                [x] -> [elemSym | check elemSym] <> x
                more -> filter check [concatSym, elemSym] <> concat more
    KListF def heads (Just (mid, tails)) ->
        let concatSym = concatSymbol $ KListMeta def
            elemSym = klistElementSymbol def
            ends = heads <> tails
         in mid
                <> if null ends
                    then []
                    else filter check [concatSym, elemSym] <> concat ends
    KSetF def elements rest ->
        let concatSym = concatSymbol $ KSetMeta def
            unitSym = unitSymbol $ KSetMeta def
            elemSym = klistElementSymbol def
         in case elements of
                [] -> fromMaybe [unitSym | check unitSym] rest
                [single] ->
                    single
                        <> maybe [elemSym | check elemSym] (filter check [concatSym, elemSym] <>) rest
                more ->
                    filter check [concatSym, elemSym] <> fromMaybe [] rest <> concat more

-- | Calculate size of a term in bytes
sizeOfTerm :: Term -> Int
sizeOfTerm = cata $ \case
    SymbolApplicationF symbol _ ts -> sum ts + BS.length symbol.name
    AndTermF t1 t2 -> t1 + t2
    DomainValueF _ v -> BS.length v
    VarF _ -> 1
    InjectionF _ _ t -> t
    KMapF _ xs mr -> sum (map (uncurry (+)) xs) + fromMaybe 0 mr
    KListF _ xs mr -> sum xs + maybe 0 (\(a, bs) -> a + sum bs) mr
    KSetF _ xs mr -> sum xs + fromMaybe 0 mr

-- | Calculate variable statistics: "variable name" -> "number of its occurrences"
termVarStats :: Term -> Map Variable Int
termVarStats = cata $ \case
    SymbolApplicationF _ _ vars -> Map.unionsWith (+) vars
    AndTermF vars1 vars2 -> Map.unionWith (+) vars1 vars2
    DomainValueF _ _ -> Map.empty
    VarF var -> Map.singleton var 1
    InjectionF _ _ t -> t
    KMapF _ xs mr ->
        Map.unionWith
            (+)
            (Map.unionsWith (+) (map (uncurry (Map.unionWith (+))) xs))
            (fromMaybe Map.empty mr)
    KListF _ xs mr ->
        Map.unionWith
            (+)
            (Map.unionsWith (+) xs)
            (maybe Map.empty (\(a, bs) -> Map.unionWith (+) a (Map.unionsWith (+) bs)) mr)
    KSetF _ xs mr ->
        Map.unionWith
            (+)
            (Map.unionsWith (+) xs)
            (fromMaybe Map.empty mr)

-- | Calculate symbol application statistics: "symbol name" -> "number of its applications"
termSymbolStats :: Term -> Map Symbol Int
termSymbolStats = cata $ \case
    SymbolApplicationF symbol _ symbols -> Map.unionsWith (+) (Map.singleton symbol 1 : symbols)
    AndTermF symbols1 symbols2 -> Map.unionWith (+) symbols1 symbols2
    DomainValueF _ _ -> Map.empty
    VarF _ -> Map.empty
    InjectionF _ _ t -> t
    KMapF _ xs mr ->
        Map.unionWith
            (+)
            (Map.unionsWith (+) (map (uncurry (Map.unionWith (+))) xs))
            (fromMaybe Map.empty mr)
    KListF _ xs mr ->
        Map.unionWith
            (+)
            (Map.unionsWith (+) xs)
            (maybe Map.empty (\(a, bs) -> Map.unionWith (+) a (Map.unionsWith (+) bs)) mr)
    KSetF _ xs mr ->
        Map.unionWith
            (+)
            (Map.unionsWith (+) xs)
            (fromMaybe Map.empty mr)

{- | Calculate symbol application statistics: "symbol name" -> "number of its applications",
     within the left-most top-most application of a specific Symbol
-}
cellSymbolStats :: SymbolName -> Term -> Map Symbol Int
cellSymbolStats name = go
  where
    go :: Term -> Map Symbol Int
    go t = case t of
        Var{} -> Map.empty
        DomainValue{} -> Map.empty
        app@(SymbolApplication sym _ args) ->
            if sym.attributes.symbolType == Constructor && sym.name == name
                then termSymbolStats app
                else Map.unionsWith (+) (map go args)
        AndTerm t1 t2 -> Map.unionsWith (+) [go t1, go t2]
        Injection _ _ sub -> go sub
        KMap{} -> Map.empty
        KList{} -> Map.empty
        KSet{} -> Map.empty

{- | Calculate variable statistics: "variable name" -> "number of its occurrences",
     within the left-most top-most application of a specific Symbol
-}
cellVariableStats :: SymbolName -> Term -> Map Variable Int
cellVariableStats name = go
  where
    go :: Term -> Map Variable Int
    go t = case t of
        Var{} -> Map.empty
        DomainValue{} -> Map.empty
        app@(SymbolApplication sym _ args) ->
            if sym.attributes.symbolType == Constructor && sym.name == name
                then termVarStats app
                else Map.unionsWith (+) (map go args)
        AndTerm t1 t2 -> Map.unionsWith (+) [go t1, go t2]
        Injection _ _ sub -> go sub
        KMap{} -> Map.empty
        KList{} -> Map.empty
        KSet{} -> Map.empty
