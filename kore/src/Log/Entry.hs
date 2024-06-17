{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Log.Entry (
    -- * Severity
    Severity (..),
    prettySeverity,

    -- * Entry
    Entry (..),
    SomeEntry (..),
    someEntry,
    entryTypeText,
) where

import Colog (
    Severity (..),
 )
import Control.Exception (
    Exception (..),
 )
import Control.Lens qualified as Lens
import Control.Lens.Prism (
    Prism,
 )
import Data.Aeson qualified as JSON
import Data.Proxy (
    Proxy (..),
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Data.Typeable qualified (
    cast,
 )
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import Type.Reflection qualified as Reflection

class (Show entry, Typeable entry) => Entry entry where
    toEntry :: entry -> SomeEntry
    toEntry = SomeEntry []

    fromEntry :: SomeEntry -> Maybe entry
    fromEntry (SomeEntry _ entry) = Data.Typeable.cast entry

    entrySeverity :: entry -> Severity

    longDoc :: entry -> Pretty.Doc ann
    default longDoc :: Pretty entry => entry -> Pretty.Doc ann
    longDoc = Pretty.pretty

    oneLineDoc :: entry -> Pretty.Doc ann

    oneLineJson :: entry -> JSON.Value
    default oneLineJson :: entry -> JSON.Value
    oneLineJson entry = JSON.object ["entry" JSON..= entryTypeText (toEntry entry)]

    oneLineContextJson :: entry -> JSON.Value
    default oneLineContextJson :: entry -> JSON.Value
    oneLineContextJson entry = JSON.object ["entry" JSON..= entryTypeText (toEntry entry)]

    oneLineContextDoc :: entry -> [Text]
    default oneLineContextDoc :: entry -> [Text]
    oneLineContextDoc = (: []) . entryTypeText . toEntry

    contextDoc :: entry -> Maybe (Pretty.Doc ann)
    contextDoc = const Nothing

    helpDoc :: Proxy entry -> Pretty.Doc ann
    helpDoc _ = Pretty.emptyDoc

    isEmpty :: entry -> Bool
    default isEmpty :: entry -> Bool
    isEmpty _ = False

data SomeEntry where
    SomeEntry :: Entry entry => [SomeEntry] -> entry -> SomeEntry

instance Show SomeEntry where
    show (SomeEntry _ entry) = show entry

instance Exception SomeEntry where
    displayException = show . longDoc

instance Entry SomeEntry where
    toEntry = id
    fromEntry = Just
    entrySeverity (SomeEntry _ entry) = entrySeverity entry
    longDoc (SomeEntry _ entry) = longDoc entry
    oneLineDoc (SomeEntry _ entry) = oneLineDoc entry
    contextDoc (SomeEntry _ entry) = contextDoc entry

someEntry :: (Entry e1, Entry e2) => Prism SomeEntry SomeEntry e1 e2
someEntry = Lens.prism' toEntry fromEntry

entryTypeText :: SomeEntry -> Text
entryTypeText (SomeEntry _ entry) =
    Text.pack . show . Reflection.typeOf $ entry

prettySeverity :: Severity -> Pretty.Doc ann
prettySeverity = Pretty.pretty . show
