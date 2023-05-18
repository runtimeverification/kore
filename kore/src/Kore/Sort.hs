{- |
Copyright   : (c) Runtime Verification, 2018-2021

Please refer to
<http://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md kore-syntax.md>.
-}
module Kore.Sort (
    SortVariable (..),
    SortActual (..),
    Sort (..),
    getSortId,
    sortSubstitution,
    substituteSortVariables,
    sameSort,
    matchSorts,

    -- * Meta-sorts
    MetaSortType (..),
    metaSort,
    metaSortTypeString,
    metaSortsListWithString,
    stringMetaSortId,
    stringMetaSortActual,
    stringMetaSort,
    predicateSortId,
    predicateSortActual,

    -- * Exceptions
    SortMismatch (..),
    sortMismatch,
    MissingArgument (..),
    missingArgument,
    UnexpectedArgument (..),
    unexpectedArgument,

    -- * Re-exports
    module Kore.Syntax.Id,
) where

import Control.Exception (
    Exception (..),
    throw,
 )
import Data.Align
import Data.Binary (Binary)
import Data.Map.Strict qualified as Map
import Data.These
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Kore.Syntax.Id
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

{- | 'SortVariable' is a Kore sort variable.

'SortVariable' corresponds to the @sort-variable@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sorts kore-syntax.md#sorts>.
-}
newtype SortVariable = SortVariable
    {getSortVariable :: Id}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SortVariable where
    unparse = unparse . getSortVariable
    unparse2 SortVariable{getSortVariable} = unparse2 getSortVariable

{- | 'SortActual' corresponds to the @sort-identifier{sorts}@ branch of the
@sort@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sorts kore-syntax.md#sorts>.
-}
data SortActual = SortActual
    { sortActualName :: !Id
    , sortActualSorts :: ![Sort]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SortActual where
    unparse SortActual{sortActualName, sortActualSorts} =
        unparse sortActualName <> parameters sortActualSorts
    unparse2 SortActual{sortActualName, sortActualSorts} =
        case sortActualSorts of
            [] -> unparse2 sortActualName
            _ ->
                "("
                    <> unparse2 sortActualName
                    <> " "
                    <> parameters2 sortActualSorts
                    <> ")"

{- | 'Sort' corresponds to the @sort@ syntactic category from
<https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sorts kore-syntax.md#sorts>.
-}
data Sort
    = SortVariableSort !SortVariable
    | SortActualSort !SortActual
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse Sort where
    unparse =
        \case
            SortVariableSort sortVariable -> unparse sortVariable
            SortActualSort sortActual -> unparse sortActual
    unparse2 =
        \case
            SortVariableSort sortVariable -> unparse2 sortVariable
            SortActualSort sortActual -> unparse2 sortActual

getSortId :: Sort -> Id
getSortId =
    \case
        SortVariableSort SortVariable{getSortVariable} ->
            getSortVariable
        SortActualSort SortActual{sortActualName} ->
            sortActualName

-- | The 'Sort' substitution from applying the given sort parameters.
sortSubstitution ::
    [SortVariable] ->
    [Sort] ->
    Map.Map SortVariable Sort
sortSubstitution variables sorts =
    foldl' insertSortVariable Map.empty (align variables sorts)
  where
    insertSortVariable map' =
        \case
            These var sort -> Map.insert var sort map'
            This _ ->
                (error . show . Pretty.vsep) ("Too few parameters:" : expected)
            That _ ->
                (error . show . Pretty.vsep) ("Too many parameters:" : expected)
    expected =
        [ "Expected:"
        , Pretty.indent 4 (parameters variables)
        , "but found:"
        , Pretty.indent 4 (parameters sorts)
        ]

{- | Substitute sort variables in a 'Sort'.

Sort variables that are not in the substitution are not changed.
-}
substituteSortVariables ::
    -- | Sort substitution
    Map.Map SortVariable Sort ->
    Sort ->
    Sort
substituteSortVariables substitution sort =
    case sort of
        SortVariableSort var ->
            fromMaybe sort $ Map.lookup var substitution
        SortActualSort sortActual@SortActual{sortActualSorts} ->
            SortActualSort
                sortActual
                    { sortActualSorts =
                        substituteSortVariables substitution <$> sortActualSorts
                    }

{- | Ths is not represented directly in the AST, we're using the string
representation instead.
-}
data MetaSortType
    = StringSort
    deriving stock (GHC.Generic, Eq)
    deriving anyclass (Binary)

instance Hashable MetaSortType

metaSortsListWithString :: [MetaSortType]
metaSortsListWithString = [StringSort]

metaSortTypeString :: MetaSortType -> String
metaSortTypeString StringSort = "String"

instance Show MetaSortType where
    show sortType = '#' : metaSortTypeString sortType

metaSort :: MetaSortType -> Sort
metaSort = \case
    StringSort -> stringMetaSort

stringMetaSortId :: Id
stringMetaSortId = implicitId "#String"

stringMetaSortActual :: SortActual
stringMetaSortActual = SortActual stringMetaSortId []

stringMetaSort :: Sort
stringMetaSort = SortActualSort stringMetaSortActual

predicateSortId :: Id
predicateSortId = implicitId "_PREDICATE"

predicateSortActual :: SortActual
predicateSortActual = SortActual predicateSortId []

{- | Placeholder sort for constructing new predicates.

The final predicate sort is unknown until the predicate is attached to a
pattern.
-}
data SortMismatch = SortMismatch !Sort !Sort
    deriving stock (Eq, Show, Typeable)

instance Exception SortMismatch where
    displayException (SortMismatch sort1 sort2) =
        (show . Pretty.vsep)
            [ "Could not make sort"
            , Pretty.indent 4 (unparse sort2)
            , "match sort"
            , Pretty.indent 4 (unparse sort1)
            , "This is a program bug!"
            ]

-- | Throw a 'SortMismatch' exception.
sortMismatch :: Sort -> Sort -> a
sortMismatch sort1 sort2 = throw (SortMismatch sort1 sort2)

newtype MissingArgument = MissingArgument Sort
    deriving stock (Eq, Show, Typeable)

instance Exception MissingArgument where
    displayException (MissingArgument sort1) =
        (show . Pretty.sep)
            [ "Expected another argument of sort"
            , unparse sort1
            ]

newtype UnexpectedArgument = UnexpectedArgument Sort
    deriving stock (Eq, Show, Typeable)

instance Exception UnexpectedArgument where
    displayException (UnexpectedArgument sort2) =
        (show . Pretty.sep)
            [ "Unexpected argument of sort"
            , unparse sort2
            ]

missingArgument :: Sort -> a
missingArgument sort1 = throw (MissingArgument sort1)

unexpectedArgument :: Sort -> a
unexpectedArgument sort2 = throw (UnexpectedArgument sort2)

-- | Throw an error if two sorts are not the same, or return the first sort.
sameSort :: Sort -> Sort -> Sort
sameSort sort1 sort2
    | sort1 == sort2 = sort1
    | otherwise = sortMismatch sort1 sort2

matchSorts :: [Sort] -> [Sort] -> [Sort]
matchSorts = alignWith matchTheseSorts
  where
    matchTheseSorts (This sort1) = missingArgument sort1
    matchTheseSorts (That sort2) = unexpectedArgument sort2
    matchTheseSorts (These sort1 sort2) = sameSort sort1 sort2
