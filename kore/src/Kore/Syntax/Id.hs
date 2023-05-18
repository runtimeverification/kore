{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

Please refer to <http://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md kore-syntax.md>.
-}
module Kore.Syntax.Id (
    -- * Identifiers
    Id (..),
    pattern Id,
    getId,
    idLocation,
    getIdForError,
    noLocationId,
    implicitId,
    generatedId,

    -- * Locations
    AstLocation (..),
    FileLocation (..),
    prettyPrintAstLocation,
) where

import Data.Binary (Binary)
import Data.InternedText
import Data.String (
    IsString (..),
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

{- | 'Id' is a Kore identifier.

'Id' corresponds to the @identifier@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#identifiers kore-syntax.md#identifiers>.
-}
data Id = InternedId
    { getInternedId :: !InternedText
    , internedIdLocation :: !AstLocation
    }
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

-- | A pattern synonym to abstract away `InternedText`.
pattern Id :: Text -> AstLocation -> Id
pattern Id{getId, idLocation} <-
    InternedId (internedText -> getId) idLocation
    where
        Id text location = InternedId (internText text) location

{-# COMPLETE Id #-}

-- | 'Ord' ignores the 'AstLocation'
instance Ord Id where
    compare first second =
        compare (getInternedId first) (getInternedId second)
    {-# INLINE compare #-}

-- | 'Eq' ignores the 'AstLocation'
instance Eq Id where
    first == second = getInternedId first == getInternedId second
    {-# INLINE (==) #-}

-- | 'Hashable' ignores the 'AstLocation'
instance Hashable Id where
    hashWithSalt salt iden = hashWithSalt salt $ getInternedId iden
    {-# INLINE hashWithSalt #-}

instance Diff Id where
    diffPrec a b =
        diffPrecGeneric
            a{idLocation = AstLocationNone}
            b{idLocation = AstLocationNone}

instance IsString Id where
    fromString = noLocationId . fromString

instance Unparse Id where
    unparse = Pretty.pretty . getId
    unparse2 = Pretty.pretty . getId

{- | Create an 'Id' without location.

Before doing this, you should consider using an existing case or adding a new
constructor to 'AstLocation'.
-}
noLocationId :: Text -> Id
noLocationId name = Id name AstLocationNone

-- | Create an implicit 'Id'.
implicitId :: Text -> Id
implicitId name = Id name AstLocationImplicit

{- | Create a generated 'Id'.

The location will be 'AstLocationGeneratedVariable'.
-}
generatedId :: Text -> Id
generatedId text =
    Id{getId = text, idLocation = location}
  where
    location = AstLocationGeneratedVariable

-- | Get the identifier name for an error message 'String'.
getIdForError :: Id -> String
getIdForError = Text.unpack . getId

{- | 'AstLocation' represents the origin of an AST node.

Its representation may change, e.g. the `AstLocationFile` branch could become a
range instead of a single character position. You should treat the entire
AstLocation as much as possible as an opaque token, i.e. hopefully only
the kore parsing code and pretty printing code below would access
the AstLocationFile branch.
-}
data AstLocation
    = AstLocationNone
    | AstLocationImplicit
    | AstLocationGeneratedVariable
    | AstLocationTest
    | AstLocationFile !FileLocation
    | -- | This should not be used and should be eliminated in further releases
      AstLocationUnknown
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

{- | 'prettyPrintAstLocation' displays an `AstLocation` in a way that's
(sort of) user friendly.
-}
prettyPrintAstLocation :: AstLocation -> Text
prettyPrintAstLocation AstLocationNone = "<unknown location>"
prettyPrintAstLocation AstLocationImplicit = "<implicitly defined entity>"
prettyPrintAstLocation AstLocationGeneratedVariable =
    "<variable generated internally>"
prettyPrintAstLocation AstLocationTest = "<test data>"
prettyPrintAstLocation
    ( AstLocationFile
            FileLocation
                { fileName = name
                , line = line'
                , column = column'
                }
        ) =
        Text.pack name
            <> " "
            <> Text.pack (show line')
            <> ":"
            <> Text.pack (show column')
prettyPrintAstLocation AstLocationUnknown = "<unknown location>"

-- | 'FileLocation' represents a position in a source file.
data FileLocation = FileLocation
    { fileName :: !FilePath
    , line :: !Int
    , column :: !Int
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
