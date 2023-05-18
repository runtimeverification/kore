{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

Representation of conditional terms.
-}
module Kore.Internal.Conditional (
    Conditional (..),
    Condition,
    withoutTerm,
    withCondition,
    andCondition,
    fromPredicate,
    fromSubstitution,
    fromSingleSubstitution,
    assign,
    andPredicate,
    splitTerm,
    isPredicate,
    Kore.Internal.Conditional.mapVariables,
    isNormalized,
    assertNormalized,
    markPredicateSimplified,
    markPredicateSimplifiedConditional,
    setPredicateSimplified,
) where

import Data.Binary (Binary)
import Data.Map.Strict (
    Map,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
 )
import Kore.Attribute.Pattern.Simplified qualified as Attribute (
    Simplified,
 )
import Kore.Debug
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.Predicate (
    Predicate,
    unparse2WithSort,
    unparseWithSort,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.Substitution (
    Assignment,
    Substitution,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike (
    AdjSomeVariableName,
    InternalVariable,
    SomeVariable,
    Sort,
    SubstitutionOrd,
    TermLike,
    mkVar,
    termLikeSort,
 )
import Kore.Substitute
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser
import Prelude.Kore
import Pretty (
    Doc,
    Pretty (..),
 )
import Pretty qualified
import SQL qualified

{- | @Conditional@ represents a value conditioned on a predicate.

@Conditional variable child@ represents a @child@ conditioned on a
@predicate@ and @substitution@ (which is a specialized form of predicate).

The 'Applicative' instance conjoins the predicates and substitutions so that the
result is conditioned on the combined predicates of the inputs. The combined
predicate and substitution are /not/ normalized.

There is intentionally no 'Monad' instance; such an instance would have
quadratic complexity.
-}
data Conditional variable child = Conditional
    { term :: !child
    , predicate :: !(Predicate variable)
    , substitution :: !(Substitution variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

-- | A predicate and substitution without an accompanying term.
type Condition variable = Conditional variable ()

instance
    ( Debug child
    , Diff child
    , Debug variable
    , Diff variable
    , Hashable variable
    , Ord variable
    , SubstitutionOrd variable
    ) =>
    Diff (Conditional variable child)

instance
    (InternalVariable variable, Semigroup term) =>
    Semigroup (Conditional variable term)
    where
    (<>) predicated1 predicated2 = (<>) <$> predicated1 <*> predicated2
    {-# INLINE (<>) #-}

instance
    (InternalVariable variable, Monoid term) =>
    Monoid (Conditional variable term)
    where
    mempty =
        Conditional
            { term = mempty
            , predicate = Predicate.makeTruePredicate
            , substitution = mempty
            }
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance InternalVariable variable => Applicative (Conditional variable) where
    pure term =
        Conditional
            { term
            , predicate = Predicate.makeTruePredicate
            , substitution = mempty
            }

    (<*>) predicated1 predicated2 =
        Conditional
            { term = f a
            , predicate = Predicate.makeAndPredicate predicate1 predicate2
            , substitution = substitution1 <> substitution2
            }
      where
        Conditional f predicate1 substitution1 = predicated1
        Conditional a predicate2 substitution2 = predicated2

{- | 'Conditional' is equivalent to the 'Control.Comonad.Env.Env' comonad.

That is to say, @Conditional variable term@ is a @term@ together with its
environment (in this case, a conjunction of side conditions).

@extract@ returns the 'term' of the 'Conditional'.

@duplicate@ re-wraps the 'Conditional' with its own side conditions.
-}
instance Comonad (Conditional variable) where
    extract = term
    {-# INLINE extract #-}

    duplicate conditional = conditional{term = conditional}
    {-# INLINE duplicate #-}

    extend f conditional = conditional{term = f conditional}
    {-# INLINE extend #-}

instance TopBottom term => TopBottom (Conditional variable term) where
    isTop Conditional{term, predicate, substitution} =
        isTop term && isTop predicate && isTop substitution
    isBottom Conditional{term, predicate, substitution} =
        isBottom term || isBottom predicate || isBottom substitution

instance
    InternalVariable variable =>
    From (Conditional variable ()) (Predicate variable)
    where
    from Conditional{predicate, substitution} =
        Predicate.makeAndPredicate predicate $
            from substitution

instance
    InternalVariable variable =>
    From (Predicate variable) (Conditional variable ())
    where
    from predicate =
        Conditional{term = (), predicate, substitution = mempty}

instance
    InternalVariable variable =>
    From (Substitution variable) (Conditional variable ())
    where
    from substitution =
        Conditional
            { term = ()
            , predicate = Predicate.makeTruePredicate
            , substitution
            }

instance
    InternalVariable variable =>
    From (Assignment variable) (Conditional variable ())
    where
    from assignment =
        Conditional
            { term = ()
            , predicate = Predicate.makeTruePredicate
            , substitution = from assignment
            }

instance
    From from to =>
    From (Conditional variable from) (Conditional variable to)
    where
    from = fmap from

instance
    InternalVariable variable =>
    From term (Conditional variable term)
    where
    from = pure

instance
    InternalVariable variable =>
    From (Map (SomeVariable variable) (TermLike variable)) (Conditional variable ())
    where
    from =
        from @(Substitution variable) @(Conditional variable ())
            . from
                @(Map (SomeVariable variable) (TermLike variable))
                @(Substitution variable)

instance
    InternalVariable variable =>
    From (Condition variable) (MultiAnd (Predicate variable))
    where
    from = Predicate.toMultiAnd . from @(Condition _) @(Predicate _)
    {-# INLINE from #-}

instance
    ( Substitute child
    , TermType child ~ TermLike variable
    , VariableNameType child ~ variable
    , InternalVariable variable
    ) =>
    Substitute (Conditional variable child)
    where
    type TermType (Conditional variable child) = TermLike variable

    type VariableNameType (Conditional variable child) = variable

    substitute subst Conditional{term, predicate, substitution} =
        Conditional
            { term = substitute subst term
            , predicate = substitute subst predicate
            , substitution = substitute subst substitution
            }

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

prettyConditional ::
    Sort ->
    -- | term
    Doc ann ->
    -- | predicate
    Doc ann ->
    -- | substitution
    [Doc ann] ->
    Doc ann
prettyConditional sort termDoc predicateDoc substitutionDocs =
    unparseAssoc'
        andHead
        andIdent
        [ below "/* term: */" termDoc
        , below "/* predicate: */" predicateDoc
        , below
            "/* substitution: */"
            (unparseAssoc' andHead andIdent substitutionDocs)
        ]
  where
    andHead = "\\and" <> parameters' [unparse sort]
    andIdent = "\\top" <> parameters' [unparse sort] <> noArguments
    below first second = (Pretty.align . Pretty.vsep) [first, second]

prettyConditional' ::
    -- | term
    Doc ann ->
    -- | predicate
    Doc ann ->
    -- | substitution
    [Doc ann] ->
    Doc ann
prettyConditional' termDoc predicateDoc substitutionDocs =
    unparseAssoc'
        andHead
        andIdent
        [ below "/* term: */" termDoc
        , below "/* predicate: */" predicateDoc
        , below
            "/* substitution: */"
            (unparseAssoc' andHead andIdent substitutionDocs)
        ]
  where
    andHead = "\\and"
    andIdent = "\\top" <> noArguments
    below first second = (Pretty.align . Pretty.vsep) [first, second]

instance InternalVariable variable => Pretty (Conditional variable ()) where
    pretty conditional =
        pretty conditional{term = Predicate.makeTruePredicate :: Predicate variable}

instance
    InternalVariable variable =>
    Unparse (Conditional variable (TermLike variable))
    where
    unparse Conditional{term, predicate, substitution} =
        prettyConditional
            sort
            (unparse term)
            (unparseWithSort sort predicate)
            (unparseWithSort sort <$> termLikeSubstitution)
      where
        sort = termLikeSort term
        termLikeSubstitution =
            Substitution.singleSubstitutionToPredicate
                <$> Substitution.unwrap substitution

    unparse2 Conditional{term, predicate, substitution} =
        prettyConditional
            sort
            (unparse2 term)
            (unparse2WithSort sort predicate)
            (unparse2WithSort sort <$> termLikeSubstitution)
      where
        sort = termLikeSort term
        termLikeSubstitution =
            Substitution.singleSubstitutionToPredicate
                <$> Substitution.unwrap substitution

instance
    InternalVariable variable =>
    Pretty (Conditional variable (Predicate variable))
    where
    pretty Conditional{term, predicate, substitution} =
        prettyConditional'
            (pretty term)
            (pretty predicate)
            (pretty <$> termLikeSubstitution)
      where
        termLikeSubstitution =
            Substitution.singleSubstitutionToPredicate
                <$> Substitution.unwrap substitution

instance
    InternalVariable variable =>
    Pretty (Conditional variable (TermLike variable))
    where
    pretty = unparse

instance
    ( InternalVariable variable
    , SQL.Column term
    , Typeable term
    ) =>
    SQL.Table (Conditional variable term)

instance
    ( InternalVariable variable
    , SQL.Column term
    , Typeable term
    ) =>
    SQL.Column (Conditional variable term)
    where
    defineColumn = SQL.defineForeignKeyColumn
    toColumn = SQL.toForeignKeyColumn

-- | Forget the 'term', keeping only the attached conditions.
withoutTerm :: Conditional variable term -> Conditional variable ()
withoutTerm predicated = predicated{term = ()}

-- | Attach the condition to the given 'term'.
withCondition ::
    term ->
    -- | Condition
    Conditional variable () ->
    Conditional variable term
withCondition term predicated = predicated{term}

-- | Combine the conditions of both arguments, taking the 'term' of the first.
andCondition ::
    InternalVariable variable =>
    Conditional variable term ->
    Conditional variable () ->
    Conditional variable term
andCondition = (<*)

{- | Construct a 'Conditional' holding the given 'Predicate'.

The result has an empty 'Substitution'.
-}
fromPredicate ::
    InternalVariable variable =>
    Predicate variable ->
    Conditional variable ()
fromPredicate = from

{- | Construct a 'Conditional' holding the given 'Substitution'.

The result has a true 'Predicate'.
-}
fromSubstitution ::
    InternalVariable variable =>
    Substitution variable ->
    Conditional variable ()
fromSubstitution = from

{- | Construct a 'Conditional' holding a single substitution.

The result has a true 'Predicate'.
-}
fromSingleSubstitution ::
    InternalVariable variable =>
    Assignment variable ->
    Conditional variable ()
fromSingleSubstitution = from

{- | A 'Conditional' assigning the 'SomeVariable' to the 'TermLike'.

The result has a true 'Predicate'.
-}
assign ::
    InternalVariable variable =>
    SomeVariable variable ->
    TermLike variable ->
    Conditional variable ()
assign uVar term = fromSingleSubstitution (Substitution.assign uVar term)

-- | Combine the predicate with the conditions of the first argument.
andPredicate ::
    InternalVariable variable =>
    Conditional variable term ->
    Predicate variable ->
    Conditional variable term
andPredicate config predicate = config `andCondition` fromPredicate predicate

instance
    (InternalVariable variable, HasFreeVariables term variable) =>
    HasFreeVariables (Conditional variable term) variable
    where
    freeVariables Conditional{term, predicate, substitution} =
        freeVariables term
            <> freeVariables predicate
            <> freeVariables substitution

-- | Check if a Conditional can be reduced to a Predicate.
isPredicate ::
    TopBottom term =>
    Conditional variable term ->
    Bool
isPredicate Conditional{term} = isTop term

-- | Transform all variables (free and quantified) in a 'Conditional' term.
mapVariables ::
    InternalVariable variable1 =>
    InternalVariable variable2 =>
    (AdjSomeVariableName (variable1 -> variable2) -> term1 -> term2) ->
    AdjSomeVariableName (variable1 -> variable2) ->
    Conditional variable1 term1 ->
    Conditional variable2 term2
mapVariables
    mapTermVariables
    adj
    Conditional{term, predicate, substitution} =
        Conditional
            { term = mapTermVariables adj term
            , predicate = Predicate.mapVariables adj predicate
            , substitution = Substitution.mapVariables adj substitution
            }

splitTerm :: Conditional variable term -> (term, Conditional variable ())
splitTerm patt@Conditional{term} = (term, withoutTerm patt)

{- | Is the condition normalized?

The 'Substitution' must be normalized and must have been substituted into the
'Predicate'.
-}
isNormalized :: Ord variable => Conditional variable term -> Bool
isNormalized Conditional{predicate, substitution} =
    Substitution.isNormalized substitution
        && Predicate.isFreeOf predicate (Substitution.variables substitution)

-- | See also: 'isNormalized'
assertNormalized ::
    HasCallStack =>
    Ord variable =>
    Conditional variable term ->
    a ->
    a
assertNormalized Conditional{predicate, substitution} a =
    a
        & assert (Substitution.isNormalized substitution)
        & assert (Predicate.isFreeOf predicate variables)
  where
    variables = Substitution.variables substitution

{- | Marks the condition's predicate as being simplified.

Since the substitution is usually simplified, this usually marks the entire
condition as simplified. Note however, that the way in which the condition
is simplified is a combination of the predicate and substitution
simplifications. As an example, if the predicate is fully simplified,
while the substitution is simplified only for a certain side condition,
the entire condition is simplified only for that side condition.
-}
markPredicateSimplified ::
    (HasCallStack, InternalVariable variable) =>
    Conditional variable term ->
    Conditional variable term
markPredicateSimplified conditional@Conditional{predicate} =
    conditional{predicate = Predicate.markSimplified predicate}

markPredicateSimplifiedConditional ::
    (HasCallStack, InternalVariable variable) =>
    SideCondition.Representation ->
    Conditional variable term ->
    Conditional variable term
markPredicateSimplifiedConditional
    sideCondition
    conditional@Conditional{predicate} =
        conditional
            { predicate =
                Predicate.markSimplifiedConditional sideCondition predicate
            }

{- | Sets the simplified attribute for a condition's predicate.

See 'markPredicateSimplified' for details.
-}
setPredicateSimplified ::
    HasCallStack =>
    InternalVariable variable =>
    Attribute.Simplified ->
    Conditional variable term ->
    Conditional variable term
setPredicateSimplified simplified conditional@Conditional{predicate} =
    conditional{predicate = Predicate.setSimplified simplified predicate}
