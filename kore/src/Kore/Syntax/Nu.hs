{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Nu (
    Nu (..),
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Syntax.Variable
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

{- | 'Nu' corresponds to the @ν@ syntactic category from the
 Syntax of the MμL

The sort of the variable is the same as the sort of the result.
-}
data Nu variable child = Nu
    { nuVariable :: !(SetVariable variable)
    , nuChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance (Unparse variable, Unparse child) => Unparse (Nu variable child) where
    unparse Nu{nuVariable, nuChild} =
        "\\nu"
            <> parameters ([] :: [Sort])
            <> arguments' [unparse nuVariable, unparse nuChild]

    unparse2 Nu{nuVariable, nuChild} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\nu"
                , unparse2SortedVariable nuVariable
                , unparse2 nuChild
                ]
            )

instance
    Ord variable =>
    Synthetic (FreeVariables variable) (Nu variable)
    where
    synthetic Nu{nuVariable, nuChild} =
        bindVariable (inject nuVariable) nuChild
    {-# INLINE synthetic #-}

instance Synthetic Sort (Nu variable) where
    synthetic Nu{nuVariable, nuChild} =
        nuSort
            & seq (sameSort nuSort nuChild)
      where
        Variable{variableSort = nuSort} = nuVariable
    {-# INLINE synthetic #-}
