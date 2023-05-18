{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Debug (
    -- * Debug
    Debug (..),
    debugPrecGeneric,

    -- * Diff
    Diff (..),
    diffPrecEq,
    diffPrecGeneric,
    diffPrecIgnore,

    -- * Exceptions
    formatExceptionInfo,
) where

import Control.Comonad.Trans.Cofree
import Data.ByteString (
    ByteString,
 )
import Data.ByteString.Short (
    ShortByteString,
 )
import Data.Char qualified as Char
import Data.Functor.Const (
    Const,
 )
import Data.Functor.Identity (
    Identity,
 )
import Data.HashMap.Strict (
    HashMap,
 )
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (
    HashSet,
 )
import Data.HashSet qualified as HashSet
import Data.Hashable (
    Hashed,
    unhashed,
 )
import Data.Int
import Data.Kind (
    Type,
 )
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.Sequence (
    Seq,
 )
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Data.Sup
import Data.Text (
    Text,
 )
import Data.Text qualified as Text (
    unpack,
 )
import Data.Typeable (
    typeOf,
 )
import Data.Void (
    Void,
 )
import Data.Word
import GHC.Stack (
    callStack,
    prettyCallStack,
 )
import GHC.Stack qualified as GHC
import Generics.SOP (
    All,
    All2,
    Code,
    ConstructorInfo,
    FieldInfo (..),
    HasDatatypeInfo,
    I (..),
    K (..),
    NP (..),
    NS (..),
    SOP (..),
 )
import Generics.SOP qualified as SOP
import Numeric.Natural (
    Natural,
 )
import Prelude.Kore
import Pretty (
    Doc,
    (<+>),
 )
import Pretty qualified
import System.Exit (
    ExitCode (..),
 )

{- | Insert a separator between the items and enclose them with the delimiters.

When the document is grouped with 'Pretty.group' and fits on one line, the
delimiters are set off by one space,

@
[ A, B, C ]
@

Otherwise, the delimiters and separators are placed at the beginning of each
line,

@
[ A
, B
, C
]
@
-}
encloseSep ::
    -- | Left delimiter
    Doc ann ->
    -- | Right delimiter
    Doc ann ->
    -- | Separator
    Doc ann ->
    -- | Items
    [Doc ann] ->
    Doc ann
encloseSep ldelim rdelim sep =
    \case
        [] -> ldelim <> rdelim
        (doc : docs) ->
            mconcat $
                concat
                    [ [ldelim <+> doc]
                    , map ((Pretty.line' <> sep) <+>) docs
                    , [Pretty.line, rdelim]
                    ]

-- | Surround the second argument with parentheses if needed.
parens ::
    -- | Needs parentheses
    Bool ->
    Doc ann ->
    Doc ann
parens needsParens
    | needsParens = Pretty.parens
    | otherwise = id

{- | 'Debug' prints data for debugging.

'debug' should produce correct Haskell source syntax so that debugged values can
be easily loaded into GHCi, i.e. @debug@ should obey

@
  read . show . debug === id
@
-}
class Debug a where
    debug :: a -> Doc ann
    debug = \a -> debugPrec a 0

    debugPrec :: a -> Int -> Doc ann
    default debugPrec ::
        (HasDatatypeInfo a, All2 Debug (Code a)) =>
        a ->
        -- | surrounding precedence
        Int ->
        Doc ann
    debugPrec = debugPrecGeneric

    debugPrecBrief :: a -> Int -> Doc ann
    debugPrecBrief = debugPrec

debugPrecGeneric ::
    forall a ann.
    (HasDatatypeInfo a, All2 Debug (Code a)) =>
    a ->
    -- | Surrounding precedence
    Int ->
    Doc ann
debugPrecGeneric a =
    debugPrecAux constructors (debugSOP (SOP.from a))
  where
    constructors = SOP.constructorInfo . SOP.datatypeInfo $ Proxy @a

debugPrecAux ::
    forall xss ann.
    All SOP.Top xss =>
    NP ConstructorInfo xss ->
    SOP (K (Int -> Doc ann)) xss ->
    -- | Surrounding precedence
    Int ->
    Doc ann
debugPrecAux constructors (SOP sop) =
    SOP.hcollapse $ SOP.hzipWith debugConstr constructors sop

precConstr, precRecord :: Int
precConstr = 10 -- precedence of function application
precRecord = 11 -- precedence of record syntax

debugConstr ::
    -- | Constructor
    ConstructorInfo xs ->
    -- | Arguments
    NP (K (Int -> Doc ann)) xs ->
    K (Int -> Doc ann) xs
debugConstr (SOP.Constructor name) args =
    K $ \precOut ->
        parens (precOut >= precConstr && (not . null) args')
            . Pretty.nest 4
            $ Pretty.sep (name' : args')
  where
    name' = parens needsParens (Pretty.pretty name)
      where
        initial = head name
        needsParens = (not . Char.isLetter) initial && initial /= '('
    args' = map ($ precConstr) (SOP.hcollapse args)
debugConstr (SOP.Infix name _ precInfix) (K x :* K y :* Nil) =
    K $ \precOut ->
        parens (precOut >= precInfix)
            . Pretty.nest 4
            $ Pretty.sep [x precInfix, Pretty.pretty name, y precInfix]
debugConstr (SOP.Record name fields) args =
    K $ \precOut ->
        parens (precOut >= precRecord)
            . Pretty.align
            . Pretty.group
            $ mconcat
                [ Pretty.pretty name
                , Pretty.line
                , encloseSep Pretty.lbrace Pretty.rbrace Pretty.comma args'
                ]
  where
    args' = SOP.hcollapse $ SOP.hzipWith debugField fields args

    debugField :: FieldInfo x -> K (Int -> Doc ann) x -> K (Doc ann) x
    debugField (FieldInfo fieldName) (K arg) =
        K $
            Pretty.nest 4 $
                Pretty.sep
                    [ Pretty.pretty fieldName Pretty.<+> "="
                    , arg 0
                    ]

debugSOP ::
    forall xss ann.
    All2 Debug xss =>
    SOP I xss ->
    SOP (K (Int -> Doc ann)) xss
debugSOP (SOP sop) =
    SOP $ SOP.hcmap pAllDebug (SOP.hcmap pDebug (SOP.mapIK debugPrecBrief)) sop
  where
    pDebug = Proxy :: Proxy Debug
    pAllDebug = Proxy :: Proxy (All Debug)

instance Debug a => Debug [a] where
    debugPrec as _ =
        Pretty.group
            . encloseSep Pretty.lbracket Pretty.rbracket Pretty.comma
            $ map debug as

instance Debug a => Debug (NonEmpty a)

instance {-# OVERLAPS #-} Debug String where
    debugPrec a = \p -> Pretty.pretty (showsPrec p a "")

instance Debug Text where
    debugPrec a = \p -> Pretty.pretty (showsPrec p a "")

instance Debug ByteString where
    debugPrec a = \p -> Pretty.pretty (showsPrec p a "")

instance Debug ShortByteString where
    debugPrec a = \p -> Pretty.pretty (showsPrec p a "")

instance Debug Void

instance Debug ()

instance (Debug a, Debug b) => Debug (a, b)

instance Debug Natural where
    debugPrec x = \_ -> Pretty.pretty x

instance Debug Integer where
    debugPrec x = \_ -> parens (x < 0) (Pretty.pretty x)

instance Debug Int where
    debugPrec x = \_ -> parens (x < 0) (Pretty.pretty x)

instance Debug Int64 where
    debugPrec x = \_ -> parens (x < 0) (Pretty.pretty x)

instance Debug Word32 where
    debugPrec x = \_ -> Pretty.pretty x

instance Debug Word64 where
    debugPrec x = \_ -> Pretty.pretty x

instance Debug Char where
    debugPrec x = \_ -> Pretty.squotes (Pretty.pretty x)

instance Debug a => Debug (Maybe a)

instance Debug a => Debug (Sup a)

instance Debug a => Debug (Identity a)

instance (Debug a, Debug (f b)) => Debug (CofreeF f a b) where
    debugPrec cofreeF =
        -- Cannot have orphan instances of Generic and HasDatatypeInfo.
        -- Use a fake instance instead.
        debugPrecAux constructorInfoCofreeF (debugSOP $ fromCofreeF cofreeF)

constructorInfoCofreeF :: NP ConstructorInfo '[ '[x, y]]
constructorInfoCofreeF =
    constrInfo :* Nil
  where
    constrInfo = SOP.Infix ":<" SOP.RightAssociative 5

fromCofreeF :: CofreeF f a b -> SOP I '[ '[a, f b]]
fromCofreeF (a :< fb) = SOP (Z (I a :* I fb :* Nil))

instance
    (Debug a, Debug (w (CofreeF f a (CofreeT f w a)))) =>
    Debug (CofreeT f w a)
    where
    debugPrec x =
        -- Cannot have orphan instances of Generic and HasDatatypeInfo.
        -- Use a fake instance instead.
        debugPrecAux constructorInfoCofreeT (debugSOP (fromCofreeT x))

constructorInfoCofreeT :: NP ConstructorInfo '[ '[x]]
constructorInfoCofreeT =
    SOP.Record "CofreeT" (FieldInfo "runCofreeT" :* Nil)
        :* Nil

fromCofreeT :: CofreeT f w a -> SOP I '[ '[w (CofreeF f a (CofreeT f w a))]]
fromCofreeT (CofreeT x) = SOP (Z (I x :* Nil))

instance (Debug k, Debug a) => Debug (Map.Map k a) where
    debugPrec as precOut =
        (parens (precOut >= 10) . Pretty.sep)
            ["Data.Map.Strict.fromList", debug (Map.toList as)]

instance (Debug k, Debug a) => Debug (HashMap k a) where
    debugPrec as precOut =
        (parens (precOut >= 10) . Pretty.sep)
            ["Data.HashMap.Strict.fromList", debug (HashMap.toList as)]

instance Debug a => Debug (Set a) where
    debugPrec as precOut =
        (parens (precOut >= 10) . Pretty.sep)
            ["Data.Set.fromList", debug (Set.toList as)]

instance Debug a => Debug (HashSet a) where
    debugPrec as precOut =
        (parens (precOut >= 10) . Pretty.sep)
            ["Data.HashSet.fromList", debug (HashSet.toList as)]

instance Debug a => Debug (Seq a) where
    debugPrec as precOut =
        (parens (precOut >= 10) . Pretty.sep)
            ["Data.Sequence.fromList", debug (toList as)]

instance Debug a => Debug (Const a (b :: Type))

instance Debug Bool

instance (Debug a, Debug b) => Debug (Either a b)

instance Debug ExitCode

instance Debug GHC.CallStack

instance Debug GHC.SrcLoc

-- | Prints a typed hole for the function.
instance (Typeable a, Typeable b) => Debug (a -> b) where
    debugPrec f = \precOut ->
        parens (precOut > 0) $
            "_" <+> "::" <+> (Pretty.pretty . show) (typeOf f)

instance Debug a => Debug (Hashed a) where
    debugPrec h precOut =
        parens (precOut >= 10) ("Data.Hashable.hashed" <+> debug (unhashed h))

{- | 'Diff' displays the difference between values for debugging.

@diff@ and @diffPrec@ should generally display the /minimal/ difference between
two values, as far as it is practical to do so. Like 'debug', @diff@ and
@diffPrec@ should produce valid Haskell source syntax to facilitate
debugging. To elide the identical parts of two values, use holes (@_@).

A default implementation is provided for @diffPrec@ by @diffPrecGeneric@, which
only requires some instances to be derived:

> data DataType = ...
>     deriving stock (GHC.Generics.Generic)
     deriving anyclass (Binary)
>     deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
>     deriving anyclass (Debug, Diff)
-}
class Diff a where
    diff :: a -> a -> Maybe (Doc ann)
    diff a b = ($ 0) <$> diffPrec a b

    -- | Display the difference of two values. If the values are different,
    --    the difference is displayed given the surrounding precedence.
    diffPrec :: a -> a -> Maybe (Int -> Doc ann)
    default diffPrec ::
        (Debug a, HasDatatypeInfo a, All2 Diff (Code a)) =>
        a ->
        a ->
        Maybe (Int -> Doc ann)
    diffPrec = diffPrecGeneric

{- | An implementation of 'diffPrec' which ignores differences.

@diffPrecIgnore@ returns @Nothing@ for all combinations of arguments. This is
useful for types which cannot be compared, for example: functions.
-}
diffPrecIgnore :: a -> a -> Maybe (Int -> Doc ann)
diffPrecIgnore _ _ = Nothing

{- | Default implementation of 'diffPrec' for instances of 'Eq'.

For any type which is 'Eq' and 'Debug', @diffPrecEq@ provides a default
implementation of 'diffPrec'. If the values differ, the entirety of both values
is displayed using 'Debug'; this is most suitable for atomic types, like
'Integer', or short strings.
-}
diffPrecEq ::
    (Debug a, Eq a) =>
    a ->
    a ->
    Maybe (Int -> Doc ann)
diffPrecEq a b
    | a == b = Nothing
    | otherwise =
        Just $ \p ->
            Pretty.sep
                [ "{- was:"
                , debugPrec a p
                , "-}"
                , debugPrec b p
                ]

{- | Default implementation of 'diffPrec' for instances of 'Generic'.

@diffPrecGeneric@ uses the 'DatatypeInfo' of a 'Generic' type to print the
difference between two values. The arguments must also be instances of 'Debug',
which is used to print the values when they have different constructors. If they
have the same constructor, 'Generic' is used to examine its fields and minimize
the difference.
-}
diffPrecGeneric ::
    forall a ann.
    (Debug a, HasDatatypeInfo a, All2 Diff (Code a)) =>
    a ->
    a ->
    Maybe (Int -> Doc ann)
diffPrecGeneric a b =
    diffPrecSOP constructors (a, SOP.from a) (b, SOP.from b)
  where
    constructors = SOP.constructorInfo . SOP.datatypeInfo $ Proxy @a

diffPrecSOP ::
    forall a xss ann.
    (Debug a, All2 Diff xss) =>
    NP ConstructorInfo xss ->
    (a, SOP I xss) ->
    (a, SOP I xss) ->
    Maybe (Int -> Doc ann)
diffPrecSOP constructors (a, SOP aNS) (b, SOP bNS) =
    diffNS constructors aNS bNS
  where
    diffNS ::
        forall xss'.
        All2 Diff xss' =>
        NP ConstructorInfo xss' ->
        NS (NP I) xss' ->
        NS (NP I) xss' ->
        Maybe (Int -> Doc ann)
    diffNS (c :* _) (Z aNP) (Z bNP) = diffNP c aNP bNP
    diffNS (_ :* cs) (S aNS') (S bNS') = diffNS cs aNS' bNS'
    diffNS _ _ _ =
        Just $ \precOut ->
            Pretty.sep
                [ "{- was:"
                , debugPrec a precOut
                , "-}"
                , debugPrec b precOut
                ]

    diffNP ::
        forall xs.
        All Diff xs =>
        ConstructorInfo xs ->
        NP I xs ->
        NP I xs ->
        Maybe (Int -> Doc ann)
    diffNP c aNP bNP
        | anyNP (isJust . SOP.unK) cNP =
            Just $ SOP.unK $ debugConstr c (SOP.hmap (SOP.mapKK maybeHole) cNP)
        | otherwise =
            Nothing
      where
        cNP = diffNP' aNP bNP
        maybeHole = fromMaybe (const "_")

    anyNP :: forall f xs. (forall x. f x -> Bool) -> NP f xs -> Bool
    anyNP query (fx :* fxs) = query fx || anyNP query fxs
    anyNP _ Nil = False

    diffNP' ::
        forall xs.
        All Diff xs =>
        NP I xs ->
        NP I xs ->
        NP (K (Maybe (Int -> Doc ann))) xs
    diffNP' = SOP.hczipWith (Proxy @Diff) (SOP.mapIIK diffPrec)

instance Diff Bool where
    diffPrec = diffPrecEq

instance (Debug a, Diff a) => Diff [a]

instance (Debug a, Diff a) => Diff (NonEmpty a)

instance {-# OVERLAPS #-} Diff String where
    diffPrec = diffPrecEq

instance Diff ByteString where
    diffPrec = diffPrecEq

instance Diff ShortByteString where
    diffPrec = diffPrecEq

instance Diff Text where
    diffPrec = diffPrecEq

instance Diff Void where
    diffPrec = diffPrecEq

instance Diff () where
    diffPrec = diffPrecEq

instance Diff Natural where
    diffPrec = diffPrecEq

instance Diff Integer where
    diffPrec = diffPrecEq

instance Diff Int where
    diffPrec = diffPrecEq

instance Diff Int64 where
    diffPrec = diffPrecEq

instance Diff Word32 where
    diffPrec = diffPrecEq

instance Diff Word64 where
    diffPrec = diffPrecEq

instance Diff Char where
    diffPrec = diffPrecEq

instance (Debug a, Diff a) => Diff (Const a (b :: Type))

instance (Debug a, Diff a) => Diff (Maybe a)

instance (Debug a, Diff a) => Diff (Sup a)

instance (Debug a, Diff a) => Diff (Identity a)

instance (Debug a, Debug (f b), Diff a, Diff (f b)) => Diff (CofreeF f a b) where
    diffPrec x y =
        -- Cannot have orphan instances of Generic and HasDatatypeInfo.
        -- Use a fake instance instead.
        diffPrecSOP constructorInfoCofreeF (x, fromCofreeF x) (y, fromCofreeF y)

instance
    ( Debug a
    , Debug (w (CofreeF f a (CofreeT f w a)))
    , Diff a
    , Diff (w (CofreeF f a (CofreeT f w a)))
    ) =>
    Diff (CofreeT f w a)
    where
    diffPrec x y =
        -- Cannot have orphan instances of Generic and HasDatatypeInfo.
        -- Use a fake instance instead.
        diffPrecSOP constructorInfoCofreeT (x, fromCofreeT x) (y, fromCofreeT y)

instance (Debug a, Diff a) => Diff (Seq a) where
    diffPrec as bs =
        fmap wrapFromList $
            diffPrec (toList as) (toList bs)
      where
        wrapFromList diff' precOut =
            parens (precOut >= 10) $ "Data.Sequence.fromList" <+> diff' 10

instance
    (Debug key, Debug value, Diff key, Diff value) =>
    Diff (Map key value)
    where
    diffPrec as bs =
        fmap wrapFromList $ diffPrec (Map.toList as) (Map.toList bs)
      where
        wrapFromList diff' precOut =
            parens (precOut >= 10) $ "Data.Map.Strict.fromList" <+> diff' 10

instance
    (Debug key, Debug value, Diff key, Diff value) =>
    Diff (HashMap key value)
    where
    diffPrec as bs =
        fmap wrapFromList $ diffPrec (HashMap.toList as) (HashMap.toList bs)
      where
        wrapFromList diff' precOut =
            parens (precOut >= 10) $ "Data.HashMap.Strict.fromList" <+> diff' 10

instance (Debug a, Debug b, Diff a, Diff b) => Diff (a, b)

instance (Debug a, Diff a) => Diff (Set a) where
    diffPrec as bs =
        fmap wrapFromList $ diffPrec (Set.toList as) (Set.toList bs)
      where
        wrapFromList diff' precOut =
            parens (precOut >= 10) $ "Data.Set.fromList" <+> diff' 10

instance (Debug a, Diff a) => Diff (HashSet a) where
    diffPrec as bs =
        fmap wrapFromList $ diffPrec (HashSet.toList as) (HashSet.toList bs)
      where
        wrapFromList diff' precOut =
            parens (precOut >= 10) $ "Data.HashSet.fromList" <+> diff' 10

instance Diff ExitCode

instance (Debug a, Debug b, Diff a, Diff b) => Diff (Either a b)

-- | Assume all functions are the same because we cannot compare them.
instance Diff (a -> b) where
    diffPrec = diffPrecIgnore

instance (Debug a, Diff a) => Diff (Hashed a) where
    diffPrec ha hb =
        fmap wrapFromList $ diffPrec (unhashed ha) (unhashed hb)
      where
        wrapFromList diff' precOut =
            parens (precOut >= 10) $ "Data.Hashable.hashed" <+> diff' 10

formatExceptionInfo :: (HasCallStack, Monad m) => Text -> m ()
formatExceptionInfo message = do
    traceM "------------------"
    traceM (prettyCallStack callStack)
    traceM ""
    traceM (Text.unpack message)
    traceM "------------------"
