{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}

{-# LANGUAGE TemplateHaskell #-}

module Kore.Syntax.Implies
    ( Implies (..)
    ) where

import           Control.DeepSeq
                 ( NFData (..) )
import qualified Data.Deriving as Deriving
import qualified Data.Foldable as Foldable
import           Data.Function
import           Data.Hashable
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Unparser

{-|'Implies' corresponds to the @\implies@ branches of the @object-pattern@ and
@meta-pattern@ syntactic categories from the Semantics of K,
Section 9.1.4 (Patterns).

'impliesSort' is both the sort of the operands and the sort of the result.

-}
data Implies sort child = Implies
    { impliesSort   :: !sort
    , impliesFirst  :: child
    , impliesSecond :: child
    }
    deriving (Eq, Functor, Foldable, GHC.Generic, Ord, Show, Traversable)

Deriving.deriveEq1 ''Implies
Deriving.deriveOrd1 ''Implies
Deriving.deriveShow1 ''Implies

instance (Hashable sort, Hashable child) => Hashable (Implies sort child)

instance (NFData sort, NFData child) => NFData (Implies sort child)

instance SOP.Generic (Implies sort child)

instance SOP.HasDatatypeInfo (Implies sort child)

instance (Debug sort, Debug child) => Debug (Implies sort child)

instance Unparse child => Unparse (Implies Sort child) where
    unparse Implies { impliesSort, impliesFirst, impliesSecond } =
        "\\implies"
        <> parameters [impliesSort]
        <> arguments [impliesFirst, impliesSecond]

    unparse2 Implies { impliesFirst, impliesSecond } =
        Pretty.parens (Pretty.fillSep
            [ "\\implies"
            , unparse2 impliesFirst
            , unparse2 impliesSecond
            ])

instance Ord variable => Synthetic (Implies sort) (FreeVariables variable) where
    synthetic = Foldable.fold
    {-# INLINE synthetic #-}

instance Synthetic (Implies Sort) Sort where
    synthetic Implies { impliesSort, impliesFirst, impliesSecond } =
        impliesSort
        & seq (matchSort impliesSort impliesFirst)
        . seq (matchSort impliesSort impliesSecond)
    {-# INLINE synthetic #-}
