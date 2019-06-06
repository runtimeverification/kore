{- |
Module      : Kore.Builtin.Set
Description : Built-in sets
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Set as Set
@
 -}
module Kore.Builtin.Set
    ( sort
    , assertSort
    , sortDeclVerifiers
    , symbolVerifiers
    , builtinFunctions
    , Builtin
    , returnSet
    , asInternal
    , asPattern
    , asTermLike
    , expectBuiltinSet
      -- * Symbols
    , lookupSymbolIn
    , lookupSymbolDifference
    , isSymbolConcat
    , isSymbolElement
    , isSymbolUnit
      -- * Keys
    , unitKey
    , elementKey
    , concatKey
    , inKey
    , differenceKey
    , toListKey
    , sizeKey
    , intersectionKey
      -- * Unification
    , unifyEquals
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Error
                 ( MaybeT, fromMaybe )
import qualified Control.Monad.Trans as Monad.Trans
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import           Data.Reflection
                 ( Given )
import qualified Data.Reflection as Reflection
import qualified Data.Sequence as Seq
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.String
                 ( IsString )
import           Data.Text
                 ( Text )
import qualified Data.Text as Text

import           Kore.Attribute.Hook
                 ( Hook )
import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin.Bool as Bool
import           Kore.Builtin.Builtin
                 ( acceptAnySort )
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Int as Int
import qualified Kore.Builtin.List as List
import qualified Kore.Domain.Builtin as Domain
import qualified Kore.Error as Kore
import           Kore.IndexedModule.IndexedModule
                 ( VerifiedModule )
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools, sortAttributes )
import           Kore.Internal.Conditional
                 ( andCondition )
import           Kore.Internal.Pattern
                 ( Pattern )
import qualified Kore.Internal.Pattern as Pattern
import           Kore.Internal.Predicate
                 ( Predicate )
import           Kore.Internal.TermLike
import           Kore.Step.Simplification.Data as Simplifier
import           Kore.Syntax.Sentence
                 ( SentenceSort (..) )
import           Kore.Unification.Unify
                 ( MonadUnify )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
                 ( Unparse, unparseToString )
import           Kore.Variables.Fresh
                 ( FreshVariable )

{- | Builtin name of the @Set@ sort.
 -}
sort :: Text
sort = "SET.Set"

{- | Verify that the sort is hooked to the builtin @Set@ sort.

  See also: 'sort', 'Builtin.verifySort'

 -}
assertSort :: Builtin.SortVerifier
assertSort = Builtin.verifySort sort

{- | Verify that hooked sort declarations are well-formed.

  See also: 'Builtin.verifySortDecl'

 -}
sortDeclVerifiers :: Builtin.SortDeclVerifiers
sortDeclVerifiers =
    HashMap.fromList [ (sort, verifySortDecl) ]
  where
    verifySortDecl indexedModule sentenceSort attrs = do
        Builtin.verifySortDecl indexedModule sentenceSort attrs
        unitId <- Builtin.getUnitId attrs
        Builtin.assertSymbolHook indexedModule unitId unitKey
        Builtin.assertSymbolResultSort indexedModule unitId expectedSort
        elementId <- Builtin.getElementId attrs
        Builtin.assertSymbolHook indexedModule elementId elementKey
        Builtin.assertSymbolResultSort indexedModule elementId expectedSort
        concatId <- Builtin.getConcatId attrs
        Builtin.assertSymbolHook indexedModule concatId concatKey
        Builtin.assertSymbolResultSort indexedModule concatId expectedSort
        return ()
      where
        SentenceSort { sentenceSortName } = sentenceSort
        expectedSort = mkSort sentenceSortName

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

 -}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( concatKey
      , Builtin.verifySymbol assertSort [assertSort , assertSort]
      )
    , ( elementKey
      , Builtin.verifySymbol assertSort [acceptAnySort]
      )
    , ( unitKey
      , Builtin.verifySymbol assertSort []
      )
    , ( inKey
      , Builtin.verifySymbol Bool.assertSort [acceptAnySort, assertSort]
      )
    , ( differenceKey
      , Builtin.verifySymbol assertSort [assertSort, assertSort]
      )
    , ( toListKey
      , Builtin.verifySymbol List.assertSort [assertSort]
      )
    , ( sizeKey
      , Builtin.verifySymbol Int.assertSort [assertSort]
      )
    , ( intersectionKey
      , Builtin.verifySymbol assertSort [assertSort, assertSort]
      )
    ]

{- | Abort function evaluation if the argument is not a @Set@ domain value.

    If the operand pattern is not a domain value, the function is simply
    'NotApplicable'. If the operand is a domain value, but not represented by a
    'BuiltinDomainSet', it is a bug.

 -}
expectBuiltinSet
    :: MonadSimplify m
    => Text  -- ^ Context for error message
    -> TermLike variable  -- ^ Operand pattern
    -> MaybeT m (Set (TermLike Concrete))
expectBuiltinSet ctx _set = do
    _set <- Builtin.expectNormalConcreteTerm _set
    case _set of
        Builtin_ domain ->
            case domain of
                Domain.BuiltinSet Domain.InternalSet { builtinSetChild } ->
                    return builtinSetChild
                _ ->
                    Builtin.verifierBug
                    $ Text.unpack ctx ++ ": Domain value is not a set"
        _ -> empty

returnSet
    :: (MonadSimplify m, Ord variable)
    => Sort
    -> Set (TermLike Concrete)
    -> m (AttemptedAxiom variable)
returnSet resultSort set = do
    tools <- Simplifier.askMetadataTools
    Builtin.appliedFunction
        $ Pattern.fromTermLike
        $ asInternal tools resultSort set

evalElement :: Builtin.Function
evalElement =
    Builtin.functionEvaluator evalElement0
  where
    evalElement0 _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom
            (case arguments of
                [_elem] -> do
                    _elem <- Builtin.expectNormalConcreteTerm _elem
                    returnSet resultSort (Set.singleton _elem)
                _ -> Builtin.wrongArity elementKey
            )

evalIn :: Builtin.Function
evalIn =
    Builtin.functionEvaluator evalIn0
  where
    evalIn0 :: Builtin.FunctionImplementation
    evalIn0 _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom $ do
            let (_elem, _set) =
                    case arguments of
                        [_elem, _set] -> (_elem, _set)
                        _ -> Builtin.wrongArity inKey
            _elem <- Builtin.expectNormalConcreteTerm _elem
            _set <- expectBuiltinSet inKey _set
            (Builtin.appliedFunction . asExpandedBoolPattern)
                (Set.member _elem _set)
      where
        asExpandedBoolPattern = Bool.asPattern resultSort

evalUnit :: Builtin.Function
evalUnit =
    Builtin.functionEvaluator evalUnit0
  where
    evalUnit0 _ resultSort =
        \case
            [] -> returnSet resultSort Set.empty
            _ -> Builtin.wrongArity unitKey

evalConcat :: Builtin.Function
evalConcat =
    Builtin.functionEvaluator evalConcat0
  where
    ctx = concatKey
    evalConcat0 :: Builtin.FunctionImplementation
    evalConcat0 _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom $ do
            let (_set1, _set2) =
                    case arguments of
                        [_set1, _set2] -> (_set1, _set2)
                        _ -> Builtin.wrongArity concatKey
                leftIdentity = do
                    _set1 <- expectBuiltinSet ctx _set1
                    if Set.null _set1
                        then
                            Builtin.appliedFunction
                            $ Pattern.fromTermLike _set2
                        else empty
                rightIdentity = do
                    _set2 <- expectBuiltinSet ctx _set2
                    if Set.null _set2
                        then
                            Builtin.appliedFunction
                            $ Pattern.fromTermLike _set1
                        else empty
                bothConcrete = do
                    _set1 <- expectBuiltinSet ctx _set1
                    _set2 <- expectBuiltinSet ctx _set2
                    returnSet resultSort (_set1 <> _set2)
            leftIdentity <|> rightIdentity <|> bothConcrete

evalDifference :: Builtin.Function
evalDifference =
    Builtin.functionEvaluator evalDifference0
  where
    ctx = differenceKey
    evalDifference0 :: Builtin.FunctionImplementation
    evalDifference0 _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom $ do
            let (_set1, _set2) =
                    case arguments of
                        [_set1, _set2] -> (_set1, _set2)
                        _ -> Builtin.wrongArity differenceKey
                rightIdentity = do
                    _set2 <- expectBuiltinSet ctx _set2
                    if Set.null _set2
                        then
                            Builtin.appliedFunction
                            $ Pattern.fromTermLike _set1
                        else empty
                bothConcrete = do
                    _set1 <- expectBuiltinSet ctx _set1
                    _set2 <- expectBuiltinSet ctx _set2
                    returnSet resultSort (Set.difference _set1 _set2)
            rightIdentity <|> bothConcrete

evalToList :: Builtin.Function
evalToList = Builtin.functionEvaluator evalToList0
  where
    evalToList0 :: Builtin.FunctionImplementation
    evalToList0 _ resultSort arguments =
        Builtin.getAttemptedAxiom $ do
            let _set =
                        case arguments of
                            [_set] -> _set
                            _      -> Builtin.wrongArity toListKey
            _set <- expectBuiltinSet toListKey _set
            List.returnList resultSort
                . fmap fromConcrete
                . Seq.fromList
                . Set.toList
                $ _set

evalSize :: Builtin.Function
evalSize = Builtin.functionEvaluator evalSize0
  where
    evalSize0 :: Builtin.FunctionImplementation
    evalSize0 _ resultSort arguments =
        Builtin.getAttemptedAxiom $ do
            let _set =
                        case arguments of
                            [_set] -> _set
                            _      -> Builtin.wrongArity sizeKey
            _set <- expectBuiltinSet sizeKey _set
            Builtin.appliedFunction
                . Int.asPattern resultSort
                . toInteger
                . Set.size
                $ _set

evalIntersection :: Builtin.Function
evalIntersection =
    Builtin.functionEvaluator evalIntersection0
  where
    ctx = intersectionKey
    evalIntersection0 :: Builtin.FunctionImplementation
    evalIntersection0 _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom $ do
            let (_set1, _set2) =
                    case arguments of
                        [_set1, _set2] -> (_set1, _set2)
                        _ -> Builtin.wrongArity intersectionKey
            _set1 <- expectBuiltinSet ctx _set1
            _set2 <- expectBuiltinSet ctx _set2
            returnSet resultSort (Set.intersection _set1 _set2)

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text Builtin.Function
builtinFunctions =
    Map.fromList
        [ (concatKey, evalConcat)
        , (elementKey, evalElement)
        , (unitKey, evalUnit)
        , (inKey, evalIn)
        , (differenceKey, evalDifference)
        , (toListKey, evalToList)
        , (sizeKey, evalSize)
        , (intersectionKey, evalIntersection)
        ]

{- | Render a 'Set' as an internal domain value pattern of the given sort.

The result sort must be hooked to the builtin @Set@ sort. The pattern will use
the internal representation of concrete 'Set' domain values; it will not use a
valid external representation. Use 'asPattern' to construct an externally-valid
pattern.

 -}
asInternal
    :: Ord variable
    => SmtMetadataTools Attribute.Symbol
    -> Sort
    -> Set (TermLike Concrete)
    -> TermLike variable
asInternal tools builtinSetSort builtinSetChild =
    (mkBuiltin . Domain.BuiltinSet)
        Domain.InternalSet
            { builtinSetSort
            , builtinSetUnit =
                Builtin.lookupSymbolUnit tools builtinSetSort attrs
            , builtinSetElement =
                Builtin.lookupSymbolElement tools builtinSetSort attrs
            , builtinSetConcat =
                Builtin.lookupSymbolConcat tools builtinSetSort attrs
            , builtinSetChild
            }
  where
    attrs = sortAttributes tools builtinSetSort

{- | Render an 'Domain.InternalSet' as a 'TermLike' domain value.
 -}
asTermLike
    :: Ord variable
    => Domain.InternalSet (TermLike Concrete)
    -> TermLike variable
asTermLike builtin
  | Set.null set = unit
  | otherwise = foldr1 concat' (element <$> Foldable.toList set)
  where
    Domain.InternalSet { builtinSetSort = builtinSort } = builtin
    Domain.InternalSet { builtinSetChild = set } = builtin
    Domain.InternalSet { builtinSetUnit = unitSymbol } = builtin
    Domain.InternalSet { builtinSetElement = elementSymbol } = builtin
    Domain.InternalSet { builtinSetConcat = concatSymbol } = builtin

    apply = mkApplySymbol builtinSort
    unit = apply unitSymbol []
    element elem' = apply elementSymbol [fromConcrete elem']
    concat' set1 set2 = apply concatSymbol [set1, set2]

{- | Render a 'Seq' as an extended domain value pattern.

    See also: 'asPattern'

 -}
asPattern
    ::  ( Ord variable
        , Given (SmtMetadataTools Attribute.Symbol)
        )
    => Sort
    -> Set (TermLike Concrete)
    -> Pattern variable
asPattern resultSort =
    Pattern.fromTermLike . asInternal tools resultSort
  where
    tools :: SmtMetadataTools Attribute.Symbol
    tools = Reflection.given

concatKey :: IsString s => s
concatKey = "SET.concat"

elementKey :: IsString s => s
elementKey = "SET.element"

unitKey :: IsString s => s
unitKey = "SET.unit"

inKey :: IsString s => s
inKey = "SET.in"

differenceKey :: IsString s => s
differenceKey = "SET.difference"

toListKey :: IsString s => s
toListKey = "SET.set2list"

sizeKey :: IsString s => s
sizeKey = "SET.size"

intersectionKey :: IsString s => s
intersectionKey = "SET.intersection"

{- | Find the symbol hooked to @SET.get@ in an indexed module.
 -}
lookupSymbolIn
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolIn = Builtin.lookupSymbol inKey

{- | Find the symbol hooked to @SET.difference@ in an indexed module.
 -}
lookupSymbolDifference
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolDifference = Builtin.lookupSymbol differenceKey

{- | Check if the given symbol is hooked to @SET.concat@.
 -}
isSymbolConcat
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolConcat = Builtin.isSymbol concatKey

{- | Check if the given symbol is hooked to @SET.element@.
 -}
isSymbolElement
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolElement = Builtin.isSymbol elementKey

{- | Check if the given symbol is hooked to @SET.unit@.
-}
isSymbolUnit
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolUnit = Builtin.isSymbol "SET.unit"

{- | Simplify the conjunction or equality of two concrete Set domain values.

    When it is used for simplifying equality, one should separately solve the
    case ⊥ = ⊥. One should also throw away the term in the returned pattern.

    The sets are assumed to have the same sort, but this is not checked. If
    multiple sorts are hooked to the same builtin domain, the verifier should
    reject the definition.
 -}
unifyEquals
    ::  forall variable unifier
    .   ( SortedVariable variable
        , Unparse variable
        , Show variable
        , FreshVariable variable
        , MonadUnify unifier
        )
    => SimplificationType
    -> SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap
    -- ^ Map from axiom IDs to axiom evaluators
    -> (TermLike variable -> TermLike variable -> unifier (Pattern variable))
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
unifyEquals
    simplificationType
    tools
    _substitutionSimplifier
    _simplifier
    _
    unifyEqualsChildren
    first
    second
  =
    unifyEquals0 first second
  where
    hookTools = Attribute.hook <$> tools

    -- | Unify the two argument patterns.
    unifyEquals0
        :: TermLike variable
        -> TermLike variable
        -> MaybeT unifier (Pattern variable)
    unifyEquals0
        (Builtin_ (Domain.BuiltinSet builtin1))
        (Builtin_ (Domain.BuiltinSet builtin2))
      =
        Monad.Trans.lift (unifyEqualsConcrete builtin1 builtin2)

    unifyEquals0
        dv1@(Builtin_ (Domain.BuiltinSet builtin1))
        app2@(App_ symbol2 args2)
      | isSymbolConcat hookTools symbol2 =
        Monad.Trans.lift
           (case args2 of
                [Builtin_ (Domain.BuiltinSet builtin2), x@(Var_ _)] ->
                    unifyEqualsFramed builtin1 builtin2 x
                [x@(Var_ _), Builtin_ (Domain.BuiltinSet builtin2)] ->
                    unifyEqualsFramed builtin1 builtin2 x
                [App_ symbol3 [ key3 ], x]
                  | isSymbolElement hookTools symbol3 ->
                        unifyEqualsSelect builtin1 symbol3 key3 x
                [x, App_ symbol3 [ key3 ]]
                  | isSymbolElement hookTools symbol3 ->
                        unifyEqualsSelect builtin1 symbol3 key3 x
                _ ->
                    Builtin.unifyEqualsUnsolved
                        simplificationType
                        dv1
                        app2
           )
      | isSymbolElement hookTools symbol2 =
        Monad.Trans.lift
            (case args2 of
                [ key2 ] ->
                    -- The key is not concrete yet, or SET.element would
                    -- have evaluated to a domain value.
                    unifyEqualsElement builtin1 key2
                _ ->
                    Builtin.wrongArity "SET.element"
            )
      | isSymbolUnit hookTools symbol2 =
        Monad.Trans.lift $ case args2 of
            [] -> unifyEqualsUnit builtin1
            _ -> Builtin.wrongArity "SET.unit"
      | otherwise =
        (error . unlines)
            [ "Unimplemented set unification for domain value vs application. "
            , "dv=" ++ unparseToString dv1
            , "app=" ++ unparseToString app2
            ]
      where
        -- Unify one concrete set with a select pattern (x:elem s:set)
        -- Note that x an s can be proper symbolic patterns (not just variables)
        -- TODO(traiansf): move it from here once the otherwise is not needed
        unifyEqualsSelect
            :: Domain.InternalSet (TermLike Concrete) -- ^ concrete set
            -> Symbol                                 -- ^ 'element' symbol
            -> TermLike variable                      -- ^ key
            -> TermLike variable                      -- ^ framing variable
            -> unifier (Pattern variable)
        unifyEqualsSelect builtin1' _ key2 set2
          | set1 == Set.empty = bottomWithExplanation
          | otherwise =
            Reflection.give tools $ do
                concreteKey1 <- Monad.Unify.scatter (Set.toList set1)
                let
                    remainderSet = Set.delete concreteKey1 set1
                    remainderSetPat = asInternal tools sort1 remainderSet
                    originalSetPat = asInternal tools sort1 set1
                    key1 = fromConcrete concreteKey1
                elemUnifier <- unifyEqualsChildren key1 key2
                simplifiedElemUnifier <- simplify elemUnifier


                unifySetSplit sort1 simplifiedElemUnifier remainderSetPat set2
                    <|> unifySetSplit
                        sort1 simplifiedElemUnifier originalSetPat set2
          where
            Domain.InternalSet
                { builtinSetChild = set1
                , builtinSetSort = sort1
                } = builtin1'

        unifySetSplit
            :: Sort
            -> Pattern variable
            -> TermLike variable
            -> TermLike variable
            -> unifier (Pattern variable)
        unifySetSplit setSort elemUnifier setPat set2 = do
            setUnifier <- unifyEqualsChildren setPat set2

            let
                setUnifierTerm :: TermLike variable
                setUnifierPredicate :: Predicate variable
                (setUnifierTerm, setUnifierPredicate) =
                    Pattern.splitTerm setUnifier
                setUnifierSet :: Set (TermLike Concrete)
                setUnifierSet = case setUnifierTerm of
                    (Builtin_
                        (Domain.BuiltinSet
                            Domain.InternalSet { builtinSetChild }
                        )
                     ) -> builtinSetChild
                    _ -> (error . unlines)
                        [ "Unexpected set unification term."
                        , "set2=" ++ unparseToString set2
                        , "setPat=" ++ unparseToString builtin1
                        , "unifier=" ++ unparseToString setUnifier
                        ]

                result :: Pattern variable
                result =
                    addElemPatternToSet
                        tools setSort elemUnifier setUnifierSet

            return (result `andCondition` setUnifierPredicate)

    unifyEquals0 app_@(App_ _ _) builtin_@(Builtin_ _) =
        unifyEquals0 builtin_ app_

    unifyEquals0 _ _ = empty

    -- | Unify two concrete sets
    unifyEqualsConcrete
        :: Domain.InternalSet (TermLike Concrete)
        -> Domain.InternalSet (TermLike Concrete)
        -> unifier (Pattern variable)
    unifyEqualsConcrete builtin1 builtin2
      | set1 == set2 = return unified
      | otherwise = bottomWithExplanation
      where
        Domain.InternalSet { builtinSetSort } = builtin1
        Domain.InternalSet { builtinSetChild = set1 } = builtin1
        Domain.InternalSet { builtinSetChild = set2 } = builtin2
        unified =
            Reflection.give tools
            $ asPattern builtinSetSort set1

    -- | Unify one concrete set with one framed concrete set.
    unifyEqualsFramed
        :: Domain.InternalSet (TermLike Concrete) -- ^ concrete set
        -> Domain.InternalSet (TermLike Concrete) -- ^ framed concrete set
        -> TermLike variable                      -- ^ framing variable
        -> unifier (Pattern variable)
    unifyEqualsFramed builtin1 builtin2 var
      | Set.isSubsetOf set2 set1 =
        -- TODO: take all possible subsets
        Reflection.give tools $ do
            let minSet = Set.difference set1 set2
            additional <- Monad.Unify.scatter $ Set.toList (Set.powerSet set2)
            remainder <-
                unifyEqualsChildren var
                $ asInternal tools builtinSetSort
                $ Set.union additional minSet
            -- Return the concrete set, but capture any predicates and
            -- substitutions from unifying the framing variable.
            return $
                asPattern builtinSetSort set1
                    `andCondition` Pattern.withoutTerm remainder

      | otherwise = bottomWithExplanation
      where
        Domain.InternalSet { builtinSetSort } = builtin1
        Domain.InternalSet { builtinSetChild = set1 } = builtin1
        Domain.InternalSet { builtinSetChild = set2 } = builtin2

    unifyEqualsElement
        :: Domain.InternalSet (TermLike Concrete) -- ^ concrete set
        -> TermLike variable                      -- ^ key
        -> unifier (Pattern variable)
    unifyEqualsElement builtin1 key2 =
        case Set.toList set1 of
            [fromConcrete -> key1] -> do
                elemUnifier <- unifyEqualsChildren key1 key2
                simplifiedElemUnifier <- simplify elemUnifier
                return $ addElemPatternToSet
                    tools sort1 simplifiedElemUnifier Set.empty

            _ -> bottomWithExplanation
      where
        Domain.InternalSet { builtinSetSort = sort1 } = builtin1
        Domain.InternalSet { builtinSetChild = set1 } = builtin1

    unifyEqualsUnit
        :: Domain.InternalSet (TermLike Concrete) -- ^ concrete set
        -> unifier (Pattern variable)
    unifyEqualsUnit builtin1 =
        if null set1
            then return
                (Pattern.fromTermLike
                    (mkBuiltin $ Domain.BuiltinSet builtin1)
                )
            else bottomWithExplanation
            -- Cannot unify a non-element Set with an element Set
      where
        Domain.InternalSet { builtinSetChild = set1 } = builtin1

    simplify :: Pattern variable -> unifier (Pattern variable)
    simplify patt =
        let (term, predicate) = Pattern.splitTerm patt
        in Monad.Unify.liftBranchedSimplifier
            $ simplifyConditionalTerm term predicate

    bottomWithExplanation :: unifier (Pattern variable)
    bottomWithExplanation = do
        Monad.Unify.explainBottom
            "Cannot unify sets with different sizes."
            first
            second
        empty

addElemPatternToSet
    :: forall variable
    .   ( Ord variable
        , Show variable
        , SortedVariable variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> Sort
    -> Pattern variable
    -> Set (TermLike Concrete)
    -> Pattern variable
addElemPatternToSet tools sort1 elemPattern existingSet =
    pure newBuiltin `andCondition` elemPredicate
  where
    elemTerm :: TermLike variable
    elemPredicate :: Predicate variable
    (elemTerm, elemPredicate) = Pattern.splitTerm elemPattern

    concreteElemTerm :: TermLike Concrete
    concreteElemTerm =
        fromMaybe
            ((error . unlines)
                [ "Unexpected variable in concrete element"
                , "elem=" ++ unparseToString elemPattern
                ]
            )
            (asConcrete elemTerm)

    newBuiltin :: TermLike variable
    newBuiltin =
        asInternal
            tools
            sort1
            (Set.insert concreteElemTerm existingSet)
