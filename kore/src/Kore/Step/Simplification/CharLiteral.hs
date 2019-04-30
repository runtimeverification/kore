{-|
Module      : Kore.Step.Simplification.CharLiteral
Description : Tools for CharLiteral pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.CharLiteral
    ( simplify
    ) where

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Step.OrPattern
                 ( OrPattern )
import qualified Kore.Step.OrPattern as OrPattern
import qualified Kore.Step.Pattern as Pattern
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )
import           Kore.Syntax.CharLiteral

{-| 'simplify' simplifies a 'CharLiteral' pattern, which means returning
an or containing a term made of that literal.
-}
simplify
    :: Ord variable
    => CharLiteral
    -> (OrPattern Meta variable, SimplificationProof Meta)
simplify (CharLiteral char) =
    ( OrPattern.fromPattern $ Pattern.fromTermLike $ mkCharLiteral char
    , SimplificationProof
    )
