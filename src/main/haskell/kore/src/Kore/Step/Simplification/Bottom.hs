{-|
Module      : Kore.Simplification.Bottom
Description : Tools for Bottom pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Bottom
    ( simplify
    ) where

import           Kore.AST.Common
                 ( Bottom (..) )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )

{-| simplifies a Bottom pattern, which means returning an always-false or.
-}
simplify
    :: Bottom level child
    -> ( OrOfExpandedPattern level domain variable
       , ()
       )
simplify Bottom {} =
    (OrOfExpandedPattern.make [], ())
