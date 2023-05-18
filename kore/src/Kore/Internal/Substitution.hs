{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Internal.Substitution (
    Substitution,
    -- Constructor for Assignment not exported
    -- on purpose
    Assignment,
    assign,
    pattern Assignment,
    assignmentToPair,
    assignedVariable,
    assignedTerm,
    mapAssignedTerm,
    retractAssignment,
    retractAssignmentFor,
    singleSubstitutionToPredicate,
    UnwrappedSubstitution,
    mkUnwrappedSubstitution,
    unwrap,
    toMap,
    toMultiMap,
    normalOrder,
    fromMap,
    singleton,
    wrap,
    modify,
    Kore.Internal.Substitution.mapVariables,
    mapTerms,
    mapAssignmentVariables,
    isNormalized,
    isSimplified,
    isSimplifiedSomeCondition,
    forgetSimplified,
    markSimplified,
    simplifiedAttribute,
    null,
    variables,
    unsafeWrap,
    unsafeWrapFromAssignments,
    Kore.Internal.Substitution.filter,
    partition,
    orderRenameAndRenormalizeTODO,
    toPredicate,
    Normalization (..),
    wrapNormalization,
    mkNormalization,
    applyNormalized,
    orientSubstitution,
    -- Constructor for UnorderedAssignment
    -- not exported on purpose
    UnorderedAssignment,
    pattern UnorderedAssignment,
) where

import Data.Binary (Binary)
import Data.List qualified as List
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import ErrorContext
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables,
    freeVariable,
    freeVariables,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Attribute.Pattern.Simplified qualified as Attribute (
    Simplified (..),
 )
import Kore.Debug
import Kore.Internal.Predicate (
    Predicate,
    pattern PredicateEquals,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.TermLike (
    TermLike,
    mkVar,
    pattern Var_,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable
import Kore.Substitute
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    Unparse,
    unparse,
    unparseToString,
 )
import Prelude.Kore hiding (
    null,
 )
import Pretty (
    Pretty,
 )
import Pretty qualified
import SQL qualified

data Assignment variable = Assignment_
    { assignedVariable :: !(SomeVariable variable)
    , assignedTerm :: !(TermLike variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance (Ord variable, Unparse variable) => Pretty (Assignment variable) where
    pretty Assignment_{assignedVariable, assignedTerm} =
        Pretty.vsep
            [ Pretty.hsep ["assigned variable:", unparse assignedVariable]
            , "assigned term:"
            , Pretty.indent 4 (unparse assignedTerm)
            ]

{- | Smart constructor for 'Assignment'. It enforces the invariant
 that for variable renaming, the smaller variable will be on the
 left of the substitution.
-}
assign ::
    (Hashable variable, Ord variable, SubstitutionOrd variable) =>
    SomeVariable variable ->
    TermLike variable ->
    Assignment variable
assign variable term =
    uncurry Assignment_ $ curry normalOrder variable term

pattern Assignment ::
    SomeVariable variable ->
    TermLike variable ->
    Assignment variable
pattern Assignment assignedVariable assignedTerm <-
    Assignment_{assignedVariable, assignedTerm}
{-# COMPLETE Assignment #-}

assignmentToPair ::
    Assignment variable ->
    (SomeVariable variable, TermLike variable)
assignmentToPair (Assignment variable term) =
    (variable, term)

mapAssignedTerm ::
    InternalVariable variable =>
    (TermLike variable -> TermLike variable) ->
    Assignment variable ->
    Assignment variable
mapAssignedTerm f (Assignment variable term) =
    assign variable (f term)

mkUnwrappedSubstitution ::
    (Hashable variable, Ord variable, SubstitutionOrd variable) =>
    [(SomeVariable variable, TermLike variable)] ->
    [Assignment variable]
mkUnwrappedSubstitution = fmap (uncurry assign)

{- | Extract an 'Assignment' from a 'Predicate' of the proper form.

Returns 'Nothing' if the 'Predicate' is not in the correct form.
-}
retractAssignment ::
    InternalVariable variable =>
    Predicate variable ->
    Maybe (Assignment variable)
retractAssignment (PredicateEquals (Var_ var) term) =
    Just (assign var term)
retractAssignment (PredicateEquals term (Var_ var)) =
    Just (assign var term)
retractAssignment _ = Nothing

-- | Wrapper for 'Assignment's which are unordered.
newtype UnorderedAssignment variable
    = UnorderedAssignment_ (Assignment variable)

pattern UnorderedAssignment ::
    SomeVariable variable ->
    TermLike variable ->
    UnorderedAssignment variable
pattern UnorderedAssignment assignedVariable assignedTerm <-
    UnorderedAssignment_ Assignment_{assignedVariable, assignedTerm}
{-# COMPLETE UnorderedAssignment #-}

{- | Smart constructor for 'UnorderedAssignment'. Note that it does not enforce
 the order invariant 'Assignment' does.
-}
unorderedAssign ::
    SomeVariable variable ->
    TermLike variable ->
    UnorderedAssignment variable
unorderedAssign variable term =
    UnorderedAssignment_ $
        Assignment_ variable term

{- | Extract an 'UnorderedAssignment' for a /particular/ variable.

Returns 'Nothing' if the 'Predicate' is not in the correct form or the variable
name does not match.

See also: 'retractAssignment'
-}
retractAssignmentFor ::
    InternalVariable variable =>
    SomeVariableName variable ->
    Predicate variable ->
    Maybe (UnorderedAssignment variable)
retractAssignmentFor someVariableName predicate =
    case predicate of
        PredicateEquals (Var_ var) term
            | variableName var == someVariableName ->
                Just $ unorderedAssign var term
        PredicateEquals term (Var_ var)
            | variableName var == someVariableName ->
                Just $ unorderedAssign var term
        _ -> Nothing

{- | @Substitution@ represents a collection @[xᵢ=φᵢ]@ of substitutions.

Individual substitutions are a pair of type

@
(SomeVariable variable, TermLike variable)
@

A collection of substitutions @[xᵢ=φᵢ]@ is /normalized/ if, for all @xⱼ=φⱼ@ in
the collection, @xⱼ@ is unique among all @xᵢ@ and none of the @xᵢ@ (including
@xⱼ@) occur free in @φⱼ@.
-}
data Substitution variable
    = -- TODO (thomas.tuegel): Instead of a sum type, use a product containing the
      -- normalized and denormalized parts of the substitution together. That
      -- would enable us to keep more substitutions normalized in the Semigroup
      -- instance below.
      Substitution ![Assignment variable]
    | NormalizedSubstitution
        !(Map (SomeVariable variable) (TermLike variable))
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

-- | 'Eq' does not differentiate normalized and denormalized 'Substitution's.
instance
    (Hashable variable, Ord variable, SubstitutionOrd variable) =>
    Eq (Substitution variable)
    where
    (==) = on (==) unwrap

-- | 'Ord' does not differentiate normalized and denormalized 'Substitution's.
instance
    (Hashable variable, Ord variable, SubstitutionOrd variable) =>
    Ord (Substitution variable)
    where
    compare = on compare unwrap

instance
    (Debug variable, Diff variable, Hashable variable, Ord variable, SubstitutionOrd variable) =>
    Diff (Substitution variable)
    where
    diffPrec a b =
        wrapDiffPrec <$> on diffPrec unwrap a b
      where
        wrapDiffPrec diff' = \precOut ->
            (if precOut >= 10 then Pretty.parens else id)
                ("Kore.Internal.Substitution.wrap" Pretty.<+> diff' 10)

instance (Ord variable, SubstitutionOrd variable, Hashable variable) => Hashable (Substitution variable) where
    hashWithSalt salt (Substitution denorm) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` denorm
    hashWithSalt salt (NormalizedSubstitution norm) =
        salt `hashWithSalt` (1 :: Int) `hashWithSalt` Map.toList norm

instance TopBottom (Substitution variable) where
    isTop = null
    isBottom _ = False

instance InternalVariable variable => Semigroup (Substitution variable) where
    a <> b
        | null a, null b = mempty
        | null a = b
        | null b = a
        | otherwise = Substitution (unwrap a <> unwrap b)

instance InternalVariable variable => Monoid (Substitution variable) where
    mempty = NormalizedSubstitution mempty

instance InternalVariable variable => SQL.Column (Substitution variable) where
    defineColumn tableName _ =
        SQL.defineColumn tableName (SQL.Proxy @(Predicate variable))
    toColumn = SQL.toColumn . toPredicate

instance
    InternalVariable variable =>
    From
        (Map (SomeVariable variable) (TermLike variable))
        (Substitution variable)
    where
    from = wrap . mkUnwrappedSubstitution . Map.toList

instance
    InternalVariable variable =>
    From (SomeVariable variable, TermLike variable) (Substitution variable)
    where
    from = uncurry singleton

instance From (UnwrappedSubstitution variable) (Substitution variable) where
    from = wrap

instance
    InternalVariable variable =>
    From (Substitution variable) (UnwrappedSubstitution variable)
    where
    from = unwrap

instance From (Assignment variable) (Substitution variable) where
    from assignment = wrap [assignment]

instance
    InternalVariable variable =>
    From (Assignment variable) (Predicate variable)
    where
    from (Assignment var patt) =
        -- Never mark this as simplified since we want to be able to rebuild the
        -- substitution sometimes (e.g. not(not(subst)) and when simplifying
        -- claims).
        Predicate.makeEqualsPredicate (TermLike.mkVar var) patt

instance
    InternalVariable variable =>
    From (Substitution variable) (Predicate variable)
    where
    from =
        Predicate.makeMultipleAndPredicate
            . fmap singleSubstitutionToPredicate
            . unwrap

instance
    InternalVariable variable =>
    From
        (Map (SomeVariableName variable) (TermLike variable))
        (Substitution variable)
    where
    from =
        fromMap . Map.fromAscList . fmap toVariable . Map.toAscList
      where
        toVariable (variableName, term) =
            let variableSort = TermLike.termLikeSort term
             in (Variable{variableName, variableSort}, term)

instance InternalVariable variable => Substitute (Substitution variable) where
    type TermType (Substitution variable) = TermLike variable

    type VariableNameType (Substitution variable) = variable

    substitute subst =
        wrap . (map . mapAssignedTerm) (substitute subst) . unwrap
    {-# INLINE substitute #-}

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

type UnwrappedSubstitution variable = [Assignment variable]

-- | Unwrap the 'Substitution' to its inner list of substitutions.
unwrap ::
    (Hashable variable, Ord variable, SubstitutionOrd variable) =>
    Substitution variable ->
    [Assignment variable]
unwrap (Substitution xs) =
    List.sortBy (on compare assignedVariable) xs
unwrap (NormalizedSubstitution xs) =
    mkUnwrappedSubstitution $ Map.toList xs

{- | Convert a normalized substitution to a 'Map'.

@toMap@ throws an error if the substitution is not normalized because the
conversion to a @Map@ would be unsound.

See also: 'fromMap'
-}
toMap ::
    HasCallStack =>
    Substitution variable ->
    Map (SomeVariableName variable) (TermLike variable)
toMap (Substitution _) =
    error "Cannot convert a denormalized substitution to a map!"
toMap (NormalizedSubstitution norm) =
    Map.mapKeysMonotonic variableName norm

toMultiMap ::
    InternalVariable variable =>
    Substitution variable ->
    Map (SomeVariable variable) (NonEmpty (TermLike variable))
toMultiMap =
    foldl' insertSubstitution Map.empty
        . fmap assignmentToPair
        . unwrap
  where
    insertSubstitution ::
        forall variable1 term.
        Ord variable1 =>
        Map variable1 (NonEmpty term) ->
        (variable1, term) ->
        Map variable1 (NonEmpty term)
    insertSubstitution multiMap (variable, termLike) =
        let push = (termLike :|) . maybe [] Prelude.Kore.toList
         in Map.alter (Just . push) variable multiMap

{- | Apply a normal order to a single substitution.

In practice, this means sorting variable-renaming substitutions.
A variable-renaming substitution is one of the forms,

@
x:S{} = y:S{}
\@X:S{} = \@Y:S{}
@

These are __not__ variable-renaming substitutions because they change variable
types:

@
x:S{} = \@Y:S{}
\@X:S{} = y:S{}
@

Variable-renaming substitutions are sorted so that the greater variable is
substituted in place of the lesser. Consistent ordering prevents variable-only
cycles.
-}
normalOrder ::
    forall variable.
    (Hashable variable, Ord variable, SubstitutionOrd variable) =>
    (SomeVariable variable, TermLike variable) ->
    (SomeVariable variable, TermLike variable)
normalOrder (uVar1, Var_ uVar2)
    | Just eVar1 <- retractElementVariable uVar1
    , Just eVar2 <- retractElementVariable uVar2
    , LT <- compareSubstitution eVar2 eVar1 =
        (uVar2, mkVar uVar1)
    | Just sVar1 <- retractSetVariable uVar1
    , Just sVar2 <- retractSetVariable uVar2
    , LT <- compareSubstitution sVar2 sVar1 =
        (uVar2, mkVar uVar1)
normalOrder subst = subst

fromMap ::
    InternalVariable variable =>
    Map (SomeVariable variable) (TermLike variable) ->
    Substitution variable
fromMap = from

{- | Construct substitution for a single variable.

The substitution is normalized if the variable does not occur free in the term.
-}
singleton ::
    InternalVariable variable =>
    SomeVariable variable ->
    TermLike variable ->
    Substitution variable
singleton var@Variable{variableName} termLike
    | TermLike.hasFreeVariable variableName termLike =
        Substitution [assign var termLike]
    | otherwise = NormalizedSubstitution (Map.singleton var termLike)

{- | Wrap the list of substitutions to an un-normalized substitution. Note that
 @wrap . unwrap@ is not @id@ because the normalization state is lost.
-}
wrap ::
    [Assignment variable] ->
    Substitution variable
wrap [] = NormalizedSubstitution Map.empty
wrap xs = Substitution xs

-- TODO(Ana): change to [Assignment] -> Substitution

{- | Wrap the list of substitutions to a normalized substitution. Do not use
 this unless you are sure you need it.
-}
unsafeWrap ::
    HasCallStack =>
    InternalVariable variable =>
    [(SomeVariable variable, TermLike variable)] ->
    Substitution variable
unsafeWrap =
    NormalizedSubstitution . List.foldl' insertNormalized Map.empty
  where
    insertNormalized subst (var, termLike) =
        Map.insert var termLike subst
            -- The variable must not occur in the substitution
            & assert (Map.notMember var subst)
            -- or in the right-hand side of this or any other substitution,
            & assert (not $ occurs termLike)
            & assert (not $ any occurs subst)
            -- this substitution must not depend on any substitution variable,
            & assert (not $ any depends $ Map.keys subst)
            -- and if this is an element variable substitution, the substitution
            -- must be defined.
            -- TODO (thomas.tuegel): isBottom -> SideCondition.isDefined
            & assert (not $ isElementVariable var && isBottom termLike)
            & withErrorContext "while wrapping substitution" (assign var termLike, unwrap $ fromMap subst)
      where
        Variable{variableName} = var
        occurs = TermLike.hasFreeVariable variableName
        depends Variable{variableName = variableName'} =
            TermLike.hasFreeVariable variableName' termLike

unsafeWrapFromAssignments ::
    HasCallStack =>
    InternalVariable variable =>
    [Assignment variable] ->
    Substitution variable
unsafeWrapFromAssignments =
    unsafeWrap . fmap assignmentToPair

{- | Maps a function over the inner representation of the 'Substitution'. The
 normalization status is reset to un-normalized.
-}
modify ::
    InternalVariable variable1 =>
    ([Assignment variable1] -> [Assignment variable2]) ->
    Substitution variable1 ->
    Substitution variable2
modify f = wrap . f . unwrap

mapAssignmentVariables ::
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Assignment variable1 ->
    Assignment variable2
mapAssignmentVariables adj (Assignment variable term) =
    assign mappedVariable mappedTerm
  where
    mappedVariable = mapSomeVariable adj variable
    mappedTerm = TermLike.mapVariables adj term

{- | 'mapVariables' changes all the variables in the substitution
 with the given function.
-}
mapVariables ::
    forall variable1 variable2.
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Substitution variable1 ->
    Substitution variable2
mapVariables adj = modify . fmap $ mapAssignmentVariables adj

mapTerms ::
    InternalVariable variable =>
    (TermLike variable -> TermLike variable) ->
    Substitution variable ->
    Substitution variable
mapTerms mapper (Substitution s) =
    Substitution (mapAssignedTerm mapper <$> s)
mapTerms mapper (NormalizedSubstitution s) =
    NormalizedSubstitution (fmap mapper s)

{- | Is the 'Substitution' fully simplified under the given side condition?

See also: 'isSimplifiedSomeCondition'.
-}
isSimplified :: SideCondition.Representation -> Substitution variable -> Bool
isSimplified _ (Substitution _) = False
isSimplified sideCondition (NormalizedSubstitution normalized) =
    all (TermLike.isSimplified sideCondition) normalized

{- | Is the 'Substitution' fully simplified under some side condition?

See also: 'isSimplified'.
-}
isSimplifiedSomeCondition :: Substitution variable -> Bool
isSimplifiedSomeCondition (Substitution _) = False
isSimplifiedSomeCondition (NormalizedSubstitution normalized) =
    all TermLike.isSimplifiedSomeCondition normalized

{- | Forget the 'simplifiedAttribute' associated with the 'Substitution'.

@
isSimplified (forgetSimplified _) == False
@
-}
forgetSimplified ::
    InternalVariable variable =>
    Substitution variable ->
    Substitution variable
forgetSimplified =
    wrap
        . fmap (mapAssignedTerm TermLike.forgetSimplified)
        . unwrap

{- | Mark a 'Substitution' as fully simplified at the current level.

See 'Kore.Internal.TermLike.markSimplified'.
-}
markSimplified ::
    InternalVariable variable =>
    Substitution variable ->
    Substitution variable
markSimplified =
    wrap
        . fmap (mapAssignedTerm TermLike.markSimplified)
        . unwrap

simplifiedAttribute ::
    Substitution variable -> Attribute.Simplified
simplifiedAttribute (Substitution _) = Attribute.NotSimplified
simplifiedAttribute (NormalizedSubstitution normalized) =
    foldMap TermLike.simplifiedAttribute normalized

-- | Returns true iff the substitution is normalized.
isNormalized :: Substitution variable -> Bool
isNormalized (Substitution _) = False
isNormalized (NormalizedSubstitution _) = True

-- | Returns true iff the substitution is empty.
null :: Substitution variable -> Bool
null (Substitution denorm) = List.null denorm
null (NormalizedSubstitution norm) = Map.null norm

-- | Filter the variables of the 'Substitution'.
filter ::
    InternalVariable variable =>
    (SomeVariable variable -> Bool) ->
    Substitution variable ->
    Substitution variable
filter filtering =
    modify (Prelude.Kore.filter (filtering . assignedVariable))

{- | Return the pair of substitutions that do and do not satisfy the criterion.

The normalization state is preserved.
-}
partition ::
    (SomeVariable variable -> TermLike variable -> Bool) ->
    Substitution variable ->
    (Substitution variable, Substitution variable)
partition criterion (Substitution substitution) =
    let (true, false) =
            List.partition
                (uncurry criterion . assignmentToPair)
                substitution
     in (Substitution true, Substitution false)
partition criterion (NormalizedSubstitution substitution) =
    let (true, false) = Map.partitionWithKey criterion substitution
     in (NormalizedSubstitution true, NormalizedSubstitution false)

-- TODO(Ana): this will be refactored in a subsequent PR
orderRenameAndRenormalizeTODO ::
    forall variable.
    InternalVariable variable =>
    SomeVariable variable ->
    Substitution variable ->
    Substitution variable
orderRenameAndRenormalizeTODO
    variable
    original@(NormalizedSubstitution substitution) =
        case reversableVars of
            [] -> original
            (v : vs) ->
                let replacementVar :: SomeVariable variable
                    replacementVar = foldr max v vs

                    replacement :: TermLike variable
                    replacement = mkVar replacementVar

                    replacedSubstitution ::
                        Map (SomeVariable variable) (TermLike variable)
                    replacedSubstitution =
                        fmap
                            ( substitute
                                (Map.singleton (variableName variable) replacement)
                            )
                            ( assertNoneAreFreeVarsInRhs
                                (Set.fromList reversableVars)
                                (Map.delete replacementVar substitution)
                            )
                 in NormalizedSubstitution
                        (Map.insert variable replacement replacedSubstitution)
      where
        reversable :: [(SomeVariable variable, TermLike variable)]
        reversable = List.filter (rhsIsVar variable) (Map.toList substitution)

        reversableVars :: [SomeVariable variable]
        reversableVars = map fst reversable

        rhsIsVar :: SomeVariable variable -> (thing, TermLike variable) -> Bool
        rhsIsVar var (_, Var_ otherVar) = var == otherVar
        rhsIsVar _ _ = False
orderRenameAndRenormalizeTODO _ substitution = substitution

assertNoneAreFreeVarsInRhs ::
    InternalVariable variable =>
    Set (SomeVariable variable) ->
    Map (SomeVariable variable) (TermLike variable) ->
    Map (SomeVariable variable) (TermLike variable)
assertNoneAreFreeVarsInRhs lhsVariables =
    fmap assertNoneAreFree
  where
    assertNoneAreFree patt =
        if Set.null commonVars
            then patt
            else
                (error . unlines)
                    [ "Unexpected lhs variable in rhs term "
                    , "in normalized substitution. While this can"
                    , "be a valid case sometimes, i.e. x=f(x),"
                    , "we don't handle that right now."
                    , "patt=" ++ unparseToString patt
                    , "commonVars="
                        ++ show
                            ( unparseToString
                                <$> Set.toList commonVars
                            )
                    ]
      where
        commonVars =
            Set.intersection lhsVariables $
                FreeVariables.toSet $
                    freeVariables patt

instance
    InternalVariable variable =>
    HasFreeVariables (Substitution variable) variable
    where
    freeVariables = foldMap freeVariablesWorker . unwrap
      where
        freeVariablesWorker (Assignment x t) =
            freeVariable x <> freeVariables t

-- | The left-hand side variables of the 'Substitution'.
variables ::
    Ord variable =>
    Substitution variable ->
    Set (SomeVariable variable)
variables (NormalizedSubstitution subst) = Map.keysSet subst
variables (Substitution subst) =
    foldMap (Set.singleton . assignedVariable) subst

{- | Apply an orientation to all variable-renaming substitutions.

A variable-renaming substitution is a pair of the form

@
X:S = Y:S
@

In a normalized substitution, the variable on the left-hand side of each
substitution pair may not appear in any other substitution pair. The order of a
variable-renaming pair is logically irrelevant, but often we have a preference
for which of @X@ and @Y@ should appear on the left-hand side (that is, we have a
preferred /orientation/).

@orientSubstitution@ applies an orientation to a normalized substitution and
yields a normalized substitution. The orientation is expressed as a function

@
SomeVariableName variable -> Bool
@

returning `True` when the named variable is preferred on the left-hand side of a
variable-renaming substitution pair. Each variable-renaming pair is oriented so
that the variable on the left-hand side is a preferred variable, if possible.
@orientSubstitution@ does not alter substitution pairs where both or neither
variable is preferred for the left-hand side.
-}
orientSubstitution ::
    forall variable.
    InternalVariable variable =>
    -- | Orientation: Is the named variable preferred on the left-hand side of
    -- variable-renaming substitution pairs?
    (SomeVariableName variable -> Bool) ->
    -- | Normalized substitution
    Map (SomeVariableName variable) (TermLike variable) ->
    Map (SomeVariableName variable) (TermLike variable)
orientSubstitution toLeft substitution =
    foldl' go substitution $ Map.toList substitution
  where
    go substitutionInProgress initialPair@(initialKey, _)
        | Just (newKey, newValue) <- retractReorderedPair initialPair =
            -- Re-orienting X = Y as Y = X.
            let newPair = Map.singleton newKey newValue
             in case Map.lookup newKey substitutionInProgress of
                    Nothing ->
                        -- There is no other Y = X substitution in the map.
                        substitutionInProgress
                            -- Remove X = Y pair.
                            & Map.delete initialKey
                            -- Apply Y = X to the right-hand side of all pairs.
                            & Map.map (substitute newPair)
                            -- Insert Y = X pair.
                            & Map.insert newKey newValue
                    Just already ->
                        -- There is a substitution Y = T in the map.
                        substitutionInProgress
                            -- Remove Y = T.
                            & Map.delete newKey
                            -- Apply Y = X to the right-hand side of all pairs.
                            & Map.map (substitute newPair)
                            -- Insert Y = X pair.
                            & Map.insert newKey newValue
                            -- Apply X = T to the right-hand side of all pairs. This
                            -- substitution never needs to be reoriented, but the reason why
                            -- is subtle:
                            -- 1. The Y = T substitution came from another swapped pair. It
                            --    was not present in the original substitution: the original
                            --    substitution was normalized, and the substitution we are
                            --    reorienting now had Y on the right-hand side, so Y was not
                            --    on the left-hand side of any pair in the original
                            --    subsitution.
                            -- 2. If Y = T came from another swapped pair, then T is not a
                            --    preferred variable.
                            -- 3. We just checked that X is not a preferred variable.
                            -- 4. Therefore, X = T is a valid orientation.
                            & Map.map
                                (substitute (Map.singleton initialKey already))
                            -- Insert X = T pair.
                            & Map.insert initialKey already
        | otherwise = substitutionInProgress

    retractReorderedPair ::
        (SomeVariableName variable, TermLike variable) ->
        Maybe (SomeVariableName variable, TermLike variable)
    retractReorderedPair (xName, TermLike.Var_ (Variable yName ySort))
        | isSameMultiplicity xName yName
        , toLeft yName
        , not (toLeft xName) =
            Just (yName, TermLike.mkVar (Variable xName ySort))
    retractReorderedPair _ = Nothing

    isSameMultiplicity ::
        SomeVariableName variable ->
        SomeVariableName variable ->
        Bool
    isSameMultiplicity x y
        | SomeVariableNameElement _ <- x, SomeVariableNameElement _ <- y = True
        | SomeVariableNameSet _ <- x, SomeVariableNameSet _ <- y = True
        | otherwise = False

{- | The result of /normalizing/ a substitution.

'normalized' holds the part of the substitution was normalized successfully.

'denormalized' holds the part of the substitution which was not normalized
because it contained simplifiable cycles.
-}
data Normalization variable = Normalization
    {normalized, denormalized :: !(UnwrappedSubstitution variable)}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup (Normalization variable) where
    (<>) a b =
        Normalization
            { normalized = on (<>) normalized a b
            , denormalized = on (<>) denormalized a b
            }

instance Monoid (Normalization variable) where
    mempty = Normalization mempty mempty

instance InternalVariable variable => Pretty (Normalization variable) where
    pretty Normalization{normalized, denormalized} =
        Pretty.vsep
            [ "normalized:"
            , (Pretty.indent 4 . Pretty.vsep) (map prettyPair normalized)
            , "denormalized:"
            , (Pretty.indent 4 . Pretty.vsep) (map prettyPair denormalized)
            ]
      where
        prettyPair assignment =
            Pretty.vsep
                [ "variable:"
                , Pretty.indent 4 (unparse $ assignedVariable assignment)
                , "term:"
                , Pretty.indent 4 (unparse $ assignedTerm assignment)
                ]

mkNormalization ::
    InternalVariable variable =>
    [(SomeVariable variable, TermLike variable)] ->
    [(SomeVariable variable, TermLike variable)] ->
    Normalization variable
mkNormalization normalized' denormalized' =
    Normalization
        (mkUnwrappedSubstitution normalized')
        (mkUnwrappedSubstitution denormalized')

wrapNormalization :: Normalization variable -> Substitution variable
wrapNormalization Normalization{normalized, denormalized} =
    wrap (normalized <> denormalized)

-- | Substitute the 'normalized' part into the 'denormalized' part.
applyNormalized ::
    InternalVariable variable =>
    Normalization variable ->
    Normalization variable
applyNormalized Normalization{normalized, denormalized} =
    Normalization
        { normalized
        , denormalized = mapAssignedTerm substitute' <$> denormalized
        }
  where
    substitute' = substitute $ Map.fromList (toSubstitution <$> normalized)
    toSubstitution (Assignment var term) = (variableName var, term)

{- | @toPredicate@ constructs a 'Predicate' equivalent to 'Substitution'.

An empty substitution list returns a true predicate. A non-empty substitution
returns a conjunction of variable-substitution equalities.
-}
toPredicate ::
    InternalVariable variable =>
    Substitution variable ->
    Predicate variable
toPredicate = from

singleSubstitutionToPredicate ::
    InternalVariable variable =>
    Assignment variable ->
    Predicate variable
singleSubstitutionToPredicate = from
