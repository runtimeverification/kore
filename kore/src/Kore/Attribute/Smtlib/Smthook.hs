{- |
Module      : Kore.Attribute.Smtlib.Smtlib
Description : SMT-HOOK translation attribute
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com

-}
module Kore.Attribute.Smtlib.Smthook
    ( Smthook (..)
    , smthookId
    ) where

import           Control.DeepSeq
                 ( NFData )
import           Data.Default
                 ( Default (..) )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Debug
import Kore.Syntax.Id
       ( Id )
import SMT.SimpleSMT
       ( SExpr )

{- | The @smthook@ attribute for symbols.

The @smthook@ attribute allows a Kore symbol and its arguments to be translated
for an external SMT solver. @smthook@ is meant to be used similarly to how
@smtlib@ is used, with the exception that it is meant for encoding into
builtin operations provided by the SMT solver and, as such, the symbol
used for encoding needs not be declared.

See 'Kore.Attribute.Smtlib.Smtlib'

 -}
newtype Smthook = Smthook { getSmthook :: Maybe SExpr }
    deriving (GHC.Generic, Eq, Ord, Show)

instance Default Smthook where
    def = Smthook Nothing

instance NFData Smthook

instance SOP.Generic Smthook

instance SOP.HasDatatypeInfo Smthook

instance Debug Smthook

-- | Kore identifier representing the @smthook@ attribute symbol.
smthookId :: Id
smthookId = "smt-hook"
