{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Internal.ApplicationSorts (
    ApplicationSorts (..),
    applicationSorts,
    symbolOrAliasSorts,
) where

import Data.Binary (Binary)
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Kore.Error
import Kore.Sort hiding (
    substituteSortVariables,
 )
import Kore.Syntax.Sentence
import Prelude.Kore

data ApplicationSorts = ApplicationSorts
    { applicationSortsOperands :: ![Sort]
    , applicationSortsResult :: !Sort
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

applicationSorts :: [Sort] -> Sort -> ApplicationSorts
applicationSorts = ApplicationSorts

{- | 'symbolOrAliasSorts' builds the return and operand sorts for an application
pattern from the given sort parameters.
-}
symbolOrAliasSorts ::
    (SentenceSymbolOrAlias sentence, MonadError (Error e) m) =>
    [Sort] ->
    sentence ->
    m ApplicationSorts
symbolOrAliasSorts params sentence = do
    variableToSort <-
        pairVariablesToSorts
            paramVariables
            params
    fullReturnSort <-
        substituteSortVariables
            (Map.fromList variableToSort)
            parametrizedReturnSort
    operandSorts <-
        mapM
            (substituteSortVariables (Map.fromList variableToSort))
            parametrizedArgumentSorts
    return
        ApplicationSorts
            { applicationSortsOperands = operandSorts
            , applicationSortsResult = fullReturnSort
            }
  where
    paramVariables = getSentenceSymbolOrAliasSortParams sentence
    parametrizedArgumentSorts = getSentenceSymbolOrAliasArgumentSorts sentence
    parametrizedReturnSort = getSentenceSymbolOrAliasResultSort sentence

substituteSortVariables ::
    MonadError (Error e) m =>
    Map SortVariable Sort ->
    Sort ->
    m Sort
substituteSortVariables variableToSort (SortVariableSort variable) =
    Map.lookup variable variableToSort
        & maybe missingSortVariable return
  where
    missingSortVariable =
        koreFail
            ( "Sort variable not found: '"
                ++ getIdForError (getSortVariable variable)
                ++ "'."
            )
substituteSortVariables
    variableToSort
    (SortActualSort sort@SortActual{sortActualSorts = sortList}) =
        do
            substituted <- mapM (substituteSortVariables variableToSort) sortList
            return (SortActualSort sort{sortActualSorts = substituted})

pairVariablesToSorts ::
    MonadError (Error e) m =>
    [SortVariable] ->
    [Sort] ->
    m [(SortVariable, Sort)]
pairVariablesToSorts variables sorts
    | variablesLength < sortsLength =
        koreFail "Application uses more sorts than the declaration."
    | variablesLength > sortsLength =
        koreFail "Application uses less sorts than the declaration."
    | otherwise = return (zip variables sorts)
  where
    variablesLength = length variables
    sortsLength = length sorts
