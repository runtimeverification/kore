{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Definition (
    parseKFileAttributes,
    KFileLocations (..),
) where

import Control.Monad.Catch (MonadThrow)
import Data.Binary (Binary)
import Data.Default qualified as Default
import Data.Generics.Product (typed)
import GHC.Generics qualified as GHC
import Kore.Attribute.Attributes (Attributes (..))
import Kore.Attribute.Parser (ParseAttributes (..))
import Kore.Attribute.SourceLocation (SourceLocation)
import Kore.Error (printError)
import Kore.Log.ErrorParse
import Prelude.Kore

{- | Location attribute for some of the K files used during execution, displayed
by the 'WarnIfLowProductivity' warning.
-}
newtype KFileLocations = KFileLocations
    {locations :: [SourceLocation]}
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)

parseKFileAttributes :: MonadThrow m => Attributes -> m SourceLocation
parseKFileAttributes (Attributes attrs) =
    case parser of
        Left err -> errorParse $ printError err
        Right sourceLocation ->
            return sourceLocation
  where
    parser = foldlM (flip parseDefinitionLocation) Default.def attrs
    parseDefinitionLocation source =
        typed @SourceLocation (parseAttribute source)
