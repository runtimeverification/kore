{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Simplify.Overloading (
    matchOverloading,
    -- for testing purposes
    unifyOverloading,
    unifyOverloadingCommonOverload,
    unifyOverloadingInjVsVariable,
    unifyOverloadingVsOverloaded,
    unifyOverloadingVsOverloadedVariable,
    OverloadingData (..),
    UnifyOverloadingResult,
    MatchOverloadingResult,
    UnifyOverloadingError (..),
    Narrowing (..),
    OverloadingResolution (..),
    MatchResult (..),
    flipResult,
) where

import Control.Monad qualified as Monad
import Control.Monad.Trans.Except (
    ExceptT,
    throwE,
 )
import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Pattern.FreeVariables qualified as Attribute
import Kore.Attribute.Synthetic (
    synthesize,
 )
import Kore.Debug
import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Symbol
import Kore.Internal.TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    mkConfigVariable,
 )
import Kore.Simplify.OverloadSimplifier
import Kore.Simplify.Simplify as Simplifier (
    Simplifier,
    askOverloadSimplifier,
 )
import Pair
import Prelude.Kore hiding (
    concat,
 )

-- | Overload solution requiring narrowing
data Narrowing variable = Narrowing
    { narrowingSubst :: !(Condition variable)
    -- ^ narrowing substitution represented as a 'Condition'
    , narrowingVars :: ![ElementVariable variable]
    -- ^ the fresh variables within the narrowingSubst
    , overloadPair :: !(Pair (TermLike variable))
    -- overload solution
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | Result of applying the 'unifyOverloading' resolution procedure
data OverloadingResolution variable
    = -- | The overloading resolves yielding the transformed pair of terms
      Simple !(Pair (TermLike variable))
    | -- | Overloading resolves, but additional narrowing is needed for solution
      WithNarrowing !(Narrowing variable)
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

data MatchResult
    = ClashResult !String
    | Resolution !(OverloadingResolution RewritingVariableName)
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

flipResult ::
    MatchResult ->
    MatchResult
flipResult result =
    case result of
        Resolution (Simple (Pair term1 term2)) ->
            Resolution (Simple (Pair term2 term1))
        _ -> result

-- | Describes the possible errors encountered during unification.
data UnifyOverloadingError
    = -- | the unification problem could not be solved by the current method
      NotApplicable
    | -- | There was a clash, unification will fail.
      --         Reason for the clash is included.
      Clash !String
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup UnifyOverloadingError where
    NotApplicable <> b = b
    a <> NotApplicable = a
    Clash a <> Clash b = Clash (a <> b)

instance Monoid UnifyOverloadingError where
    mempty = NotApplicable

type UnifyOverloadingResult unifier variable =
    ExceptT UnifyOverloadingError unifier (OverloadingResolution variable)

type MatchOverloadingResult unifier variable =
    ExceptT
        UnifyOverloadingError
        unifier
        (Pair (TermLike variable))

throwBottom :: String -> Maybe MatchResult
throwBottom = Just . ClashResult

matchOverloading ::
    Pair (TermLike RewritingVariableName) ->
    MatchOverloadingResult Simplifier RewritingVariableName
matchOverloading termPair = do
    overloadSimplifier <- askOverloadSimplifier
    case unifyOverloading overloadSimplifier termPair of
        Just OverloadingData{matchResult} -> case matchResult of
            Resolution (Simple pair) -> return pair
            ClashResult msg -> throwE $ Clash msg
            _ -> empty
        Nothing -> empty

data OverloadingData = OverloadingData
    { term1, term2 :: !(TermLike RewritingVariableName)
    , matchResult :: !MatchResult
    }

{- |
 Tests whether the pair of terms can be coerced to have the same constructors
 at the top, and, if so, returns the thus obtained new pair.

 See <https://github.com/runtimeverification/haskell-backend/blob/master/docs/2019-08-27-Unification-modulo-overloaded-constructors.md>

 If the overloading rules are not applicable, it would return @Nothing@.
 However, if it detects an unification failure, it would return a reason
 for the failure which can be used as an explanation for it.

 Note: this algorithm is used for both matching and unification; that is why
 the first and second terms in a pair are not interchangeable.
-}
unifyOverloading ::
    OverloadSimplifier ->
    Pair (TermLike RewritingVariableName) ->
    Maybe OverloadingData
unifyOverloading overloadSimplifier termPair =
    OverloadingData term1 term2 <$> case termPair of
        Pair
            (Inj_ inj@Inj{injChild = App_ firstHead firstChildren})
            secondTerm@(App_ secondHead _) ->
                flipResult
                    <$> unifyOverloadingVsOverloaded
                        overloadSimplifier
                        secondHead
                        secondTerm
                        (Application firstHead firstChildren)
                        inj{injChild = ()}
        Pair
            firstTerm@(App_ firstHead _)
            (Inj_ inj@Inj{injChild = App_ secondHead secondChildren}) ->
                unifyOverloadingVsOverloaded
                    overloadSimplifier
                    firstHead
                    firstTerm
                    (Application secondHead secondChildren)
                    inj{injChild = ()}
        Pair
            (Inj_ inj@Inj{injChild = App_ firstHead firstChildren})
            (Inj_ inj'@Inj{injChild = App_ secondHead secondChildren})
                | injFrom inj /= injFrom inj' -> -- this case should have been handled by now
                    unifyOverloadingCommonOverload
                        overloadSimplifier
                        (Application firstHead firstChildren)
                        (Application secondHead secondChildren)
                        inj{injChild = ()}
        Pair firstTerm secondTerm ->
            case worker firstTerm secondTerm of
                Nothing -> worker secondTerm firstTerm
                Just result -> Just result
  where
    Pair term1 term2 = termPair
    worker ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        Maybe MatchResult
    worker
        firstTerm@(App_ firstHead _)
        (Inj_ inj@Inj{injChild = ElemVar_ secondVar}) =
            unifyOverloadingVsOverloadedVariable
                overloadSimplifier
                firstHead
                firstTerm
                secondVar
                inj{injChild = ()}
    worker
        (Inj_ Inj{injChild = firstTerm@(App_ firstHead firstChildren)})
        (Inj_ inj@Inj{injChild = ElemVar_ secondVar}) =
            unifyOverloadingInjVsVariable
                overloadSimplifier
                (Application firstHead firstChildren)
                secondVar
                (Attribute.freeVariables firstTerm)
                inj{injChild = ()}
    worker (App_ firstHead _) (Inj_ Inj{injChild}) =
        notUnifiableTest firstHead injChild
    worker _ _ = Nothing

    notUnifiableTest termHead child = do
        Monad.guard (isOverloaded termHead)
        notUnifiableError child
      where
        OverloadSimplifier{isOverloaded} = overloadSimplifier
{-# INLINE unifyOverloading #-}

{- Handles the case
    inj{S1,injTo}(firstHead(firstChildren))
    vs.
    inj{S2,injTo}(secondHead(secondChildren))
  If there exists  a common overload headUnion such that
    inj{S1,injTo}(firstHead(firstChildren)) =
        inj{S,injTo}(headUnion(inj1(firstChildren)))
  and
    inj{S2,injTo}(secondHead(secondChildren)) =
        inj{S,injTo}(headUnion(inj2(secondChildren)))

  Then reduces the problem to
    inj{S,injTo}(headUnion(inj1(firstChildren)))
   vs
    inj{S,injTo}(headUnion(inj2(secondChildren)))
-}
unifyOverloadingCommonOverload ::
    OverloadSimplifier ->
    Application Symbol (TermLike RewritingVariableName) ->
    Application Symbol (TermLike RewritingVariableName) ->
    Inj () ->
    Maybe MatchResult
unifyOverloadingCommonOverload
    overloadSimplifier
    (Application firstHead firstChildren)
    (Application secondHead secondChildren)
    injProto@Inj{injTo}
        | isOverloaded firstHead
        , isOverloaded secondHead =
            case unifyOverloadWithinBound injProto firstHead secondHead injTo of
                Nothing -> Just $ ClashResult "overloaded constructors not unifiable"
                Just InjectedOverload{overload, injectionHead} ->
                    let first' = resolveOverloading injProto overload firstChildren
                        second' = resolveOverloading injProto overload secondChildren
                        mkInj' = maybeMkInj injectionHead
                     in Just $ Resolution $ Simple $ Pair (mkInj' first') (mkInj' second')
        | otherwise = Nothing
      where
        OverloadSimplifier
            { isOverloaded
            , resolveOverloading
            , unifyOverloadWithinBound
            } = overloadSimplifier

{- Handles the case
    overloadingTerm@(overloadingHead(overloadingChildren))
    vs.
    inj(overloadedHead(overloadedChildren))
  If there exists an overloading axiom such that
    inj(overloadedHead(overloadedChildren)) =
        overloadingHead(inj'(overloadedChildren))

  Then it reduces the initial problem to
    overloadingTerm@(overloadingHead(overloadingChildren))
   vs
    overloadingHead(inj'(overloadedChildren))
-}
unifyOverloadingVsOverloaded ::
    OverloadSimplifier ->
    Symbol ->
    TermLike RewritingVariableName ->
    Application Symbol (TermLike RewritingVariableName) ->
    Inj () ->
    Maybe MatchResult
unifyOverloadingVsOverloaded
    overloadSimplifier
    overloadingHead
    overloadingTerm
    (Application overloadedHead overloadedChildren)
    injProto
        | isOverloaded overloadingHead
        , isConstructor overloadedHead || isOverloaded overloadedHead =
            let ~overloadedTerm' =
                    resolveOverloading injProto overloadingHead overloadedChildren
             in if isOverloading overloadingHead overloadedHead
                    then Just $ Resolution $ Simple $ Pair overloadingTerm overloadedTerm'
                    else throwBottom "different injected ctor"
        | otherwise = Nothing
      where
        OverloadSimplifier
            { isOverloaded
            , isOverloading
            , resolveOverloading
            } = overloadSimplifier

{- Handles the case
    overloadingTerm@(overloadingHead(overloadingChildren))
    vs.
    inj{S2,injTo}(overloadedVariable)
  If there exists a (unique) maximum overloadedHead with its result sort S2'
  within S2 such that
    inj{S2',injTo}(overloadedHead(freshVars)) =
        overloadingHead(inj2(freshVars))

  Then it narrows overloadedVariable to inj{S2',S2}(overloadedHead(freshVars))
  and reduces the initial problem to
    overloadingTerm@(overloadingHead(overloadingChildren))
   vs
    overloadingHead(inj2(freshVars))
-}
unifyOverloadingVsOverloadedVariable ::
    OverloadSimplifier ->
    Symbol ->
    TermLike RewritingVariableName ->
    ElementVariable RewritingVariableName ->
    Inj () ->
    Maybe MatchResult
unifyOverloadingVsOverloadedVariable
    overloadSimplifier
    overloadingHead
    overloadingTerm
    overloadedVar
    injProto@Inj{injFrom} =
        do
            Monad.guard (isOverloaded overloadingHead)
            case getOverloadedWithinSort injProto overloadingHead injFrom of
                Right Nothing -> Just notUnifiableOverloads
                Right (Just overHead) ->
                    Just $
                        Resolution $
                            WithNarrowing $
                                computeNarrowing
                                    overloadSimplifier
                                    overloadingTerm
                                    Nothing
                                    overloadingHead
                                    injProto
                                    freeVars
                                    overloadedVar
                                    overHead
                Left err -> error err
      where
        freeVars = freeVariables overloadingTerm
        OverloadSimplifier{isOverloaded, getOverloadedWithinSort} = overloadSimplifier

{- Handles the case
    inj{S1,injTo}(firstHead(firstChildren))
    vs.
    inj{S2,injTo}(secondVariable)
  If there exists a (unique) maximum overloadedHead' with its result sort
  S2' within S2, such that there exists a common overload headUnion such that
    inj{S1,injTo}(firstHead(firstChildren)) =
        inj{S,injTo}(headUnion(inj1(firstChildren)))
  and
    inj{S2',injTo}(secondHead(freshVars)) =
        inj{S,injTo}(headUnion(inj2(freshVars)))

  Then it narrows overloadedVariable to inj{S2',S2}(secondHead(freshVars))
  and reduces the problem to
    inj{S,injTo}(headUnion(inj1(firstChildren)))
   vs
    inj{S,injTo}(headUnion(inj2(freshVars)))
-}
unifyOverloadingInjVsVariable ::
    OverloadSimplifier ->
    Application Symbol (TermLike RewritingVariableName) ->
    ElementVariable RewritingVariableName ->
    Attribute.FreeVariables RewritingVariableName ->
    Inj () ->
    Maybe MatchResult
unifyOverloadingInjVsVariable
    overloadSimplifier
    (Application firstHead firstChildren)
    overloadedVar
    freeVars
    injProto
        | isOverloaded firstHead =
            case unifyOverloadWithSortWithinBound firstHead injProto of
                Left err -> error err
                Right Nothing -> Just notUnifiableOverloads
                Right
                    (Just InjectedOverloadPair{overloadingSymbol, overloadedSymbol}) ->
                        let (InjectedOverload headUnion maybeInjUnion) = overloadingSymbol
                            first' = resolveOverloading injProto headUnion firstChildren
                         in Just $
                                Resolution $
                                    WithNarrowing $
                                        computeNarrowing
                                            overloadSimplifier
                                            first'
                                            maybeInjUnion
                                            headUnion
                                            injProto
                                            freeVars
                                            overloadedVar
                                            overloadedSymbol
        | otherwise = Nothing
      where
        OverloadSimplifier
            { isOverloaded
            , resolveOverloading
            , unifyOverloadWithSortWithinBound
            } = overloadSimplifier

computeNarrowing ::
    HasCallStack =>
    -- | overloading simpifier
    OverloadSimplifier ->
    -- | overloading pair LHS
    TermLike RewritingVariableName ->
    -- | optional injection
    Maybe (Inj ()) ->
    -- | overloading symbol
    Symbol ->
    -- | injection to overloading symbol
    Inj () ->
    -- | free vars in the unification pair
    Attribute.FreeVariables RewritingVariableName ->
    -- | injected variable (to be narrowed)
    ElementVariable RewritingVariableName ->
    -- | overloaded symbol injected into the variable's sort
    InjectedOverload ->
    Narrowing RewritingVariableName
computeNarrowing
    overloadSimplifier
    first'
    injection'
    headUnion
    injUnion
    freeVars
    overloadedVar
    overloaded
        | App_ _ freshTerms <- overloadedTerm =
            let second' =
                    resolveOverloading injUnion headUnion freshTerms
             in Narrowing
                    { narrowingSubst =
                        Condition.assign (inject overloadedVar) narrowingTerm
                    , narrowingVars =
                        Attribute.getFreeElementVariables $ freeVariables narrowingTerm
                    , overloadPair = Pair (mkInj' first') (mkInj' second')
                    }
        | otherwise = error "This should not happen"
      where
        InjectedOverload{overload, injectionHead} = overloaded
        allVars = Attribute.freeVariable (inject overloadedVar) <> freeVars
        overloadedTerm = freshSymbolInstance allVars overload "x"
        mkInj' = maybeMkInj injection'
        narrowingTerm = maybeMkInj injectionHead overloadedTerm
        OverloadSimplifier{resolveOverloading} = overloadSimplifier

-- | Generates fresh variables as arguments for a symbol to create a pattern.
freshSymbolInstance ::
    FreeVariables RewritingVariableName ->
    Symbol ->
    Text ->
    TermLike RewritingVariableName
freshSymbolInstance freeVars sym base =
    mkApplySymbol sym varTerms
        & refreshVariables freeVars
  where
    sorts = applicationSortsOperands $ symbolSorts sym
    varTerms = mkElemVar <$> zipWith mkVariable [1 ..] sorts

    mkVariable :: Integer -> Sort -> ElementVariable RewritingVariableName
    mkVariable vIdx vSort =
        mkElementVariable
            (generatedId $ base <> (Text.pack . show) vIdx)
            vSort
            & (fmap . fmap) mkConfigVariable

mkInj ::
    Inj () ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
mkInj inj injChild = (synthesize . InjF) inj{injChild}

maybeMkInj ::
    Maybe (Inj ()) ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
maybeMkInj maybeInj injChild = maybe injChild (flip mkInj injChild) maybeInj

notUnifiableError ::
    TermLike RewritingVariableName -> Maybe MatchResult
notUnifiableError = \case
    (DV_ _ _) -> Just $ ClashResult "injected domain value"
    (InternalBool_ _) -> Just $ ClashResult "injected builtin bool"
    (InternalInt_ _) -> Just $ ClashResult "injected builtin int"
    (InternalList_ _) -> Just $ ClashResult "injected builtin list"
    (InternalMap_ _) -> Just $ ClashResult "injected builtin map"
    (InternalSet_ _) -> Just $ ClashResult "injected builtin set"
    (InternalString_ _) -> Just $ ClashResult "injected builtin string"
    _ -> Nothing

notUnifiableOverloads :: MatchResult
notUnifiableOverloads = ClashResult "overloaded constructors not unifiable"
