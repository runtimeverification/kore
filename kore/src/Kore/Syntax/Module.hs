{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Module (
    ModuleName (..),
    getModuleNameForError,
    Module (..),
) where

import Data.Aeson (
    FromJSON,
    ToJSON,
 )
import Data.Binary (Binary)
import Data.Kind (
    Type,
 )
import Data.String (
    IsString,
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Attributes
import Kore.Debug
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

-- | 'ModuleName' corresponds to the @module-name@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#identifiers kore-syntax#identifiers>.
newtype ModuleName = ModuleName {getModuleName :: Text}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving newtype (IsString, FromJSON, ToJSON)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse ModuleName where
    unparse = Pretty.pretty . getModuleName
    unparse2 = Pretty.pretty . getModuleName

getModuleNameForError :: ModuleName -> String
getModuleNameForError = Text.unpack . getModuleName

{- | A 'Module' consists of a 'ModuleName' a list of 'Sentence's and some
'Attributes'.

They correspond to the second, third and forth non-terminals of the @definition@
syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#definition-modules kore-syntax#definition-modues>.
-}
data Module (sentence :: Type) = Module
    { moduleName :: !ModuleName
    , moduleSentences :: ![sentence]
    , moduleAttributes :: !Attributes
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Debug sentence => Debug (Module sentence)

instance (Debug sentence, Diff sentence) => Diff (Module sentence)

instance Unparse sentence => Unparse (Module sentence) where
    unparse
        Module{moduleName, moduleSentences, moduleAttributes} =
            (Pretty.vsep . catMaybes)
                [ Just ("module" Pretty.<+> unparse moduleName)
                , case moduleSentences of
                    [] -> Nothing
                    _ ->
                        (Just . Pretty.indent 4 . Pretty.vsep)
                            (unparse <$> moduleSentences)
                , Just "endmodule"
                , Just (unparse moduleAttributes)
                ]

    unparse2
        Module{moduleName, moduleSentences, moduleAttributes} =
            (Pretty.vsep . catMaybes)
                [ Just ("module" Pretty.<+> unparse2 moduleName)
                , case moduleSentences of
                    [] -> Nothing
                    _ ->
                        (Just . Pretty.indent 4 . Pretty.vsep)
                            (unparse2 <$> moduleSentences)
                , Just "endmodule"
                , Just (unparse2 moduleAttributes)
                ]
