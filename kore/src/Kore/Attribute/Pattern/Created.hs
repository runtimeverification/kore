{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Pattern.Created (
    Created (..),
    hasKnownCreator,
) where

import Data.Binary (Binary (..))
import GHC.Generics qualified as GHC
import GHC.Stack (
    SrcLoc (..),
 )
import GHC.Stack qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Synthetic
import Kore.Debug
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

{- | 'Created' is used for debugging patterns, specifically for finding out
 where a pattern was created. This is a field in the attributes of a pattern,
 and it will default to 'Nothing'. This field is populated via the smart
 constructors in 'Kore.Internal.TermLike'.
-}
newtype Created = Created {getCreated :: Maybe GHC.CallStack}
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance Binary Created where
    put = const $ pure ()
    get = pure $ Created Nothing

hasKnownCreator :: Created -> Bool
hasKnownCreator = isJust . getCallStackHead

instance Eq Created where
    (==) _ _ = True

instance Hashable Created where
    hashWithSalt _ _ = 0

instance Diff Created where
    diffPrec = diffPrecIgnore

instance Pretty Created where
    pretty =
        maybe "" go . getCallStackHead
      where
        go (name, srcLoc) =
            Pretty.hsep ["/* Created:", qualifiedName, "*/"]
          where
            qualifiedName =
                Pretty.pretty srcLocModule
                    <> Pretty.dot
                    <> Pretty.pretty name
            SrcLoc{srcLocModule} = srcLoc

instance Functor pat => Synthetic Created pat where
    synthetic = const (Created Nothing)

getCallStackHead :: Created -> Maybe (String, SrcLoc)
getCallStackHead Created{getCreated} =
    getCreated >>= headMay . GHC.getCallStack
