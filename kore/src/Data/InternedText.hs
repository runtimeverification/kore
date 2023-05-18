{-# LANGUAGE BlockArguments #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

An implementation of string interning.
Instead of keeping many copies of the same `Text` in memory, we keep only one shared `InternedText`.

Each `InternedText` has a unique ID. As a result:

* the `Eq` instance can more quickly check if the ID of two `InternedText`s are equal,
instead of checking every single character.
* the `Hashable` instance can hash the ID, which is a lot faster than hashing the string's contents.
-}
module Data.InternedText (
    -- * Interned text
    InternedText (..),
    internText,

    -- * Cache
    InternedTextCache (..),
    globalInternedTextCache,
) where

import Data.Binary (Binary (..))
import Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Text (
    Text,
 )
import GHC.Generics (Generic)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Prelude.Kore
import System.IO.Unsafe (unsafePerformIO)

-- | A pool containing all of the process' interned strings.
data InternedTextCache = InternedTextCache
    { internedTexts :: !(HashMap Text InternedText)
    , counter :: {-# UNPACK #-} !Word
    -- ^ An incremental counter used to generate new IDs.
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

{- | An interned `Text`.

 The constructor `UnsafeMkInternedText` should not be used directly - it's exported for testing only.
 Use `internText` instead.
-}
data InternedText = UnsafeMkInternedText
    { internedText :: {-# UNPACK #-} !Text
    , internedId :: {-# UNPACK #-} !Word
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Show InternedText where
    showsPrec prec = showsPrec prec . internedText

instance Debug InternedText where
    debugPrec = debugPrec . internedText

instance Diff InternedText where
    diffPrec = diffPrec `on` internedText

-- | Checks if the IDs are equal.
instance Eq InternedText where
    a == b = internedId a == internedId b
    {-# INLINE (==) #-}

-- | Hashes the `InternedText`'s ID.
instance Hashable InternedText where
    hashWithSalt salt = hashWithSalt salt . internedId
    {-# INLINE hashWithSalt #-}
    hash = hash . internedId
    {-# INLINE hash #-}

instance Ord InternedText where
    a `compare` b
        -- Quickly check if their interned IDs are equal.
        | internedId a == internedId b = EQ
        -- If they're not, fallback to using lexical order by comparing the strings' actual contents.
        | otherwise = internedText a `compare` internedText b
    {-# INLINE compare #-}

-- | The global cache with all of the process' interned strings.
globalInternedTextCache :: IORef InternedTextCache
globalInternedTextCache = unsafePerformIO $ newIORef $ InternedTextCache HashMap.empty 0
{-# NOINLINE globalInternedTextCache #-}

{- | If the argument text has been previously interned, the existing `InternedText` instance is returned.
If it hasn't, a new `InternedText` with a brand new ID is allocated and returned.

In other words, multiple evaluations of `internText` with the same argument
should return a pointer to the exact same `InternedText` object.

@O(log (number of interned strings))@.
-}
internText :: Text -> InternedText
internText text =
    unsafePerformIO do
        atomicModifyIORef' globalInternedTextCache \InternedTextCache{counter, internedTexts} ->
            let ((internedText, newCounter), newInternedTexts) =
                    HashMap.alterF
                        \case
                            -- If this text is already interned, reuse it.
                            existing@(Just interned) -> ((interned, counter), existing)
                            -- Otherwise, create a new ID for it and intern it.
                            Nothing ->
                                let newIden = counter
                                    !newInterned = UnsafeMkInternedText text newIden
                                 in ((newInterned, counter + 1), Just newInterned)
                        text
                        internedTexts
             in (InternedTextCache newInternedTexts newCounter, internedText)
{-# INLINE internText #-}
