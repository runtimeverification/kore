{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2023-
License     : BSD-3-Clause
-}
module Booster.Pattern.Binary (
    Version (..),
    decodeTerm,
    decodeTerm',
    decodePattern,
    encodeMagicHeaderAndVersion,
    encodePattern,
    encodeTerm,
) where

import Control.Monad (forM_, unless)
import Control.Monad.Extra (forM)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans.State (StateT, evalStateT, get, gets, modify, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (Bits (complement, shiftL, (.&.), (.|.)), shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Word (Word8)
import Prettyprinter (pretty)
import Text.Printf

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Bool (pattern TrueBool)
import Booster.Pattern.Pretty
import Booster.Pattern.Util (sortOfTerm)
import Booster.Prettyprinter (renderDefault)

-- | tags indicating the next element in a block, see @'decodeBlock'@
pattern
    KORECompositePattern
    , KOREStringPattern
    , KORECompositeSort
    , KORESortVariable
    , KORESymbol
    , KOREVariablePattern
    , KOREVariable ::
        Word8
pattern KORECompositePattern = 0x4
pattern KOREStringPattern = 0x5
pattern KORECompositeSort = 0x6
pattern KORESortVariable = 0x7
pattern KORESymbol = 0x8
pattern KOREVariablePattern = 0x9
pattern KOREVariable = 0xD

data Version = Version
    { major :: Int16
    , minor :: Int16
    , patch :: Int16
    }
    deriving (Eq, Ord)

instance Show Version where
    show version = printf "%d.%d.%d" version.major version.minor version.patch

{- | The marshalling algorithm below unpacks terms into a global term
   store, indexed by "shallow terms", i.e., where arguments in
   application nodes are replaced by indexes into a global term store.

@ShallowTerm@s can be either simple data without recursion
(@DomainValue@, @Var@), or recursive data (@SymbolApplication@,
@AndTerm@, @Injection@), where the arguments are initially @Int@s
instead of @Term@s (internal collection types are constructed from the
unpacked symbol applications later, and not expected to occur here).

A lookup map for these @ShallowTerm@ indexes is maintained while
unpacking blocks.

Unpacked @ShallowTerm@s that are not found in the lookup map are added
to both the lookup map (as @ShallowTerm@s) and to the term store (as
@Term@s, resolving arguments), and the new index is returned.

Conversely, when an unpacked @ShallowTerm@ has occurred before, the
previously-added index is returned.
-}
newtype ShallowTerm = ShallowTerm (TermF Idx)
    deriving (Eq, Ord, Show)

type Idx = Int

data Block
    = BTerm Idx
    | BPredicate Idx -- Predicate -- problem?!?
    | BString ByteString
    | BSort Sort
    | BSymbol ByteString [Sort]
    deriving (Show)

data DecoderState = DecoderState
    { internedStrings :: Map.Map Int BS.ByteString
    , stack :: [Block]
    , termStore :: Seq Term
    -- ^ remembers all unpacked terms in an append-only list. Needs to
    -- ensure subterms will be shared.
    , termCache :: Map.Map ShallowTerm Idx
    -- ^ lookup index into termStore
    }
    deriving (Show)

newtype DecodeM a = DecodeM {unDecodeM :: ReaderT (Version, Maybe KoreDefinition) (StateT DecoderState Get) a}
    deriving newtype (Functor, Applicative, Monad, MonadFail)

runDecodeM :: Version -> Maybe KoreDefinition -> DecodeM a -> Get a
runDecodeM v mDef =
    flip evalStateT (DecoderState mempty mempty mempty mempty)
        . flip runReaderT (v, mDef)
        . unDecodeM

liftDecode :: Get a -> DecodeM a
liftDecode m = DecodeM $ lift $ lift m

askVersion :: DecodeM Version
askVersion = DecodeM $ asks fst

getInternedStrings :: DecodeM (Map.Map Int BS.ByteString)
getInternedStrings = DecodeM $ lift $ gets internedStrings

insertInternedString :: Int -> BS.ByteString -> DecodeM ()
insertInternedString pos str =
    DecodeM $
        lift $
            modify (\s@DecoderState{internedStrings} -> s{internedStrings = Map.insert pos str internedStrings})

-- | Insert a new item into the term store or return a previously-seen instance
registerTerm :: ShallowTerm -> DecodeM (Idx, Term)
registerTerm shallow = DecodeM $ do
    ds@DecoderState{termCache, termStore} <- lift get
    case Map.lookup shallow termCache of
        Just idx -> pure (idx, termStore `Seq.index` idx)
        Nothing -> do
            let !new = resolve termStore shallow -- strict: fail early on inconsistent data
                newStore = termStore :|> new
                newIdx = Seq.length termStore -- NB index 0-based
                newCache = Map.insert shallow newIdx termCache
            lift $ put ds{termStore = newStore, termCache = newCache}
            pure (newIdx, new)

getTerm :: Idx -> DecodeM Term
getTerm idx = DecodeM $ do
    store <- lift $ gets termStore
    pure $ store `Seq.index` idx

{- | Resolves indexes into the term store in a shallow term, assuming
all indexes exist. Returns the resolved (full) term.
-}
resolve :: Seq Term -> ShallowTerm -> Term
resolve store (ShallowTerm shallow) = case shallow of
    AndTermF i1 i2 -> AndTerm (fromStore i1) (fromStore i2)
    SymbolApplicationF sym sorts is -> SymbolApplication sym sorts $ map fromStore is
    DomainValueF sort payload -> DomainValue sort payload
    VarF v -> Var v
    InjectionF s1 s2 i -> Injection s1 s2 (fromStore i)
    other -> error $ "Unexpected shallow term " <> show other
  where
    fromStore = Seq.index store

------------------------------------------------------------

{- | Length (non-negative integer) is encoded in one of two special
  formats (depending on the version).
  Version 1.0.x: stored big-endian in a fixed amount of bytes.
  Other versions:
    - stored little-endian using _the lower 7 bits_ of each byte
    - where bit 8 of the byte is set if the number continues. If bit 8
      is _not_ set, the decoding ends.
    - a maximum of 9 bytes is read (max. value is '2^(7 * 9)')
-}
decodeLength :: Int -> DecodeM Int
decodeLength l = do
    version <- askVersion
    liftDecode $
        if areCompatible version $ Version 1 0 0
            then readAndShift l 0x0
            else readAndShiftV2 True 0 0
  where
    areCompatible :: Version -> Version -> Bool
    areCompatible a b = a.major == b.major && a.minor == b.minor

    readAndShift :: Int -> Int -> Get Int
    readAndShift counter ret
        | counter > 0 = do
            r <- getWord8
            readAndShift (counter - 1) $ (ret `shiftL` 8) .|. fromIntegral r
        | otherwise = pure ret

    readAndShiftV2 :: Bool -> Int -> Int -> Get Int
    readAndShiftV2 _ steps _ | steps >= 9 = fail "No terminating byte in variable-length field"
    readAndShiftV2 False _ ret = pure ret
    readAndShiftV2 True steps ret = do
        chunk <- getWord8
        let contBit = 0x80
            continue = chunk .&. contBit == contBit
            chunk' = chunk .&. complement contBit
            ret' = ret .|. (fromIntegral $ (fromIntegral chunk' :: Word64) `shiftL` (7 * steps))
        readAndShiftV2 continue (steps + 1) ret'

{- | A string can either be a literal encoded as length/contents (tag
  0x01), or a back-reference to a previously-unpacked string (tag
  0x02), given as a _relative_ offset backwards from the current
  position. The previously-unpacked strings are remembered in the
  string table of the decode state to avoid having to step back.
-}
decodeString :: DecodeM BS.ByteString
decodeString = do
    liftDecode getWord8 >>= \case
        0x1 -> do
            -- record the position of the length argument, to be added to the backref map
            position <- fromIntegral <$> liftDecode bytesRead
            len <- decodeLength 4
            str <- liftDecode $ getByteString len
            insertInternedString position str
            pure str
        0x2 -> do
            backref <- decodeLength 4
            -- we look up the position after reading `backref` and subtract `backref` from it,
            -- to obtain the position of the string, which we then look up in the backref map
            position <- fromIntegral <$> liftDecode bytesRead
            m <- getInternedStrings
            case Map.lookup (position - backref) m of
                Just str -> pure str
                Nothing -> do
                    let offsets =
                            [ show n <> " -> " <> show s <> "\n"
                            | (n, s) <- Map.assocs m
                            ]
                    fail $
                        "Incorrect offset for interned string at "
                            <> show (position, backref)
                            <> " with table\n "
                            <> unwords offsets
        _ -> fail "Incorrect String encoding"

getStack :: DecodeM [Block]
getStack = DecodeM $ lift $ gets stack

popStack :: Int -> DecodeM [Block]
popStack n = DecodeM $ lift $ do
    stack <- gets stack
    unless (length stack >= n) $ fail "Trying to pop more items off the stack than available"
    modify (\s -> s{stack = drop n stack})
    pure $ reverse $ take n stack

pushStack :: Block -> DecodeM ()
pushStack t = DecodeM $ lift $ do
    modify (\s@DecoderState{stack} -> s{stack = t : stack})

popStackSorts :: Int -> DecodeM [Sort]
popStackSorts n =
    popStack n >>= \stack' -> forM stack' $ \case
        BSort s -> pure s
        _ -> fail "popping a non sort"

lookupKoreDefinitionSymbol :: SymbolName -> DecodeM (Either Symbol (Maybe Symbol))
lookupKoreDefinitionSymbol name = DecodeM $ do
    (_, mDef) <- ask
    pure $ case mDef of
        -- return a symbol with dummy attributes if no definition is supplied.
        -- this should be used for testing ONLY!
        Nothing ->
            Left $
                Symbol
                    name
                    []
                    []
                    (SortApp "UNKNOWN" [])
                    ( SymbolAttributes
                        (Function Booster.Definition.Attributes.Base.Partial)
                        IsNotIdem
                        IsNotAssoc
                        IsNotMacroOrAlias
                        CannotBeEvaluated
                        Nothing
                        Nothing
                        Nothing
                    )
        Just def -> Right $ Map.lookup name $ symbols def

{- | Successively decodes items from the given "block" of bytes,
  branching on the initial tag of the item.
  A block is a sequence of items which are encoded as described in the
  comments on each tag case below.
  Decoding adds items to an internal stack, symbol application
  consumes stack elements according to the symbol arity. At the end of
  the block, the stack is expected to hold a single element (the
  resulting term which is returned).
-}
decodeBlock :: Maybe Int -> DecodeM [Block]
decodeBlock mbSize = do
    whileNotEnded $ do
        liftDecode getWord8 >>= \case
            KORECompositePattern ->
                -- apply node (arity), uses current symbol and the stack of terms
                popStack 1 >>= \case
                    [BSymbol symbolName sorts] -> do
                        arity <- decodeLength 2
                        args <- popStack arity
                        mkSymbolApplication symbolName sorts args >>= pushStack
                    _ -> fail "No current symbol set"
            KOREStringPattern -> do
                -- either literal string (length,data) or back-reference (rel.offset)
                str <- decodeString
                pushStack $ BString str
            KORECompositeSort -> do
                -- sort constructor (arity, name)
                arity <- decodeLength 2
                sortName <- decodeString
                args <- popStackSorts arity
                pushStack $ BSort $ SortApp sortName args
            KORESortVariable -> do
                var <- decodeString
                pushStack $ BSort $ SortVar var
            KORESymbol -> do
                -- symbol applied to (sort) arguments (arity, name)
                arity <- decodeLength 2
                symbolName <- decodeString
                sorts <- popStackSorts arity
                pushStack $ BSymbol symbolName sorts
            KOREVariablePattern -> pure ()
            KOREVariable -> do
                var <- decodeString
                [sort] <- popStackSorts 1
                (idx, _) <- registerTerm $ ShallowTerm $ VarF $ Variable sort var
                pushStack $ BTerm idx
            h -> fail $ "Invalid header " <> show h

    getStack
  where
    shouldStop = case mbSize of
        Nothing ->
            liftDecode isEmpty
        Just n ->
            liftDecode bytesRead >>= \case
                size
                    | size < fromIntegral n ->
                        pure False
                    | size > fromIntegral n ->
                        fail $ "Block decoder consumed " <> show size <> " > " <> show n <> " bytes"
                    | otherwise -> -- size == fromIntegral n
                        pure True

    whileNotEnded m = do
        shouldStop >>= \case
            True -> pure ()
            False -> m >> whileNotEnded m

    -- The workhorse in this decoder
    -- The term cache is managed here, so that we can keep the
    -- distinction between terms and predicates confined.
    mkSymbolApplication :: ByteString -> [Sort] -> [Block] -> DecodeM Block
    mkSymbolApplication name sorts args = do
        store <- DecodeM $ lift $ gets termStore
        case name of
            -- This special symbol will be removed in post-processing, see below
            "rawTerm"
                | [BTerm idx] <- args
                , null sorts ->
                    returnRegistered BTerm $ SymbolApplicationF RawTermSymbol [] [idx]
                | otherwise ->
                    argError "rawTerm" [BTerm undefined] args
            -- translate many reserved "special" symbols into their
            -- respective terms or predicates.
            "\\and"
                | [BTerm t1, BTerm t2] <- args ->
                    returnRegistered BTerm $ AndTermF t1 t2
                | otherwise ->
                    argError "AndTerm" [BTerm undefined, BTerm undefined] args
            "\\dv"
                | [BString txt] <- args
                , [sort] <- sorts ->
                    returnRegistered BTerm $ DomainValueF sort txt
                | otherwise ->
                    argError "DomainValue" [BString undefined] args
            "\\equals"
                | [BTerm t1, BTerm t2] <- args
                , TrueBool <- store `Seq.index` t2 ->
                    pure $ BPredicate t1
                | [BTerm t1, BTerm t2] <- args
                , TrueBool <- store `Seq.index` t1 ->
                    pure $ BPredicate t2
                | otherwise ->
                    argError "EqualBTerm/EqualBPredicate" [BTerm undefined, BTerm undefined] args
            "inj"
                | [source, target] <- sorts
                , [BTerm t] <- args ->
                    returnRegistered BTerm $ InjectionF source target t
                | otherwise ->
                    argError "Injection" [BTerm undefined] args
            -- unsupported symbols (non-boolean predicates)
            "\\bottom" ->
                argError "Bottom" [] args
            "\\ceil" ->
                argError "Ceil" [BTerm undefined] args
            "\\exists" ->
                argError "Exists" [BTerm undefined, BPredicate undefined] args
            "\\forall" ->
                argError "Forall" [BTerm undefined, BPredicate undefined] args
            "\\iff" ->
                argError "Iff" [BPredicate undefined, BPredicate undefined] args
            "\\implies" ->
                argError "Implies" [BPredicate undefined, BPredicate undefined] args
            "\\in" ->
                argError "In" [BTerm undefined, BTerm undefined] args
            "\\not" ->
                argError "Not" [BPredicate undefined] args
            "\\or" ->
                argError "Or" [BPredicate undefined, BPredicate undefined] args
            "\\top" ->
                argError "Top" [] args
            _otherwise ->
                lookupKoreDefinitionSymbol name >>= \case
                    Left symbol@Symbol{sortVars} -> do
                        -- testing case when we don't have a KoreDefinition:
                        -- only check arguments are terms
                        idxs <- forM args $ \case
                            BTerm i -> pure i
                            _ -> fail "Expecting term"
                        returnRegistered BTerm $
                            SymbolApplicationF symbol (zipWith (const id) sortVars sorts) idxs
                    Right (Just symbol@Symbol{sortVars, argSorts}) -> do
                        -- check arguments and their sorts
                        idxs <- forM (zip argSorts args) $ \case
                            (srt, BTerm argIdx) -> do
                                trm <- getTerm argIdx
                                unless (sortOfTerm trm == srt) $
                                    fail $
                                        "Argument has incorrect sort. Expecting "
                                            <> renderDefault (pretty (PrettyWithModifiers @['Decoded, 'Truncated] srt))
                                            <> " but got "
                                            <> renderDefault (pretty (PrettyWithModifiers @['Decoded, 'Truncated] $ sortOfTerm trm))
                                pure argIdx
                            _ -> fail "Expecting term"
                        returnRegistered BTerm $
                            SymbolApplicationF symbol (zipWith (const id) sortVars sorts) idxs
                    Right Nothing ->
                        fail $ "Unknown symbol " <> show name

    returnRegistered cons shallow = do
        (idx, _) <- registerTerm $ ShallowTerm shallow
        pure $ cons idx

    argError cons expectedArgs receivedArgs =
        fail $
            "Invalid "
                <> cons
                <> "arguments\nExpected ["
                <> intercalate ", " (map typeOfArg expectedArgs)
                <> "] but got ["
                <> intercalate ", " (map typeOfArg receivedArgs)
                <> "]"

    typeOfArg = \case
        BTerm _ -> "Term"
        BPredicate _ -> "Predicate"
        BString _ -> "String"
        BSort _ -> "Sort"
        BSymbol _ _ -> "Symbol"

{- | The term in binary format is stored as follows:

Bytes   1    4      2         2         2                ?
      +----+------+---------+---------+---------------+---------+
      | 7f | KORE | <major> | <minor> | <patch-level> | <BLOCK> |
      +----+------+---------+---------+---------------+---------+
with the first byte ignored (7f) a fixed magic header "KORE", major,
minor, patch-level 16-bit integers (little-endian), and the block
encoded as described in @'decodeBlock'@.

Version 1.2.0 adds an 8-byte length field before the block.

Bytes   1    4      2         2         2               8          <length>
      +----+------+---------+---------+---------------+----------+---------+
      | 7f | KORE | <major> | <minor> | <patch-level> | <length> | <BLOCK> |
      +----+------+---------+---------+---------------+----------+---------+

This function returns the _total_ size (including the header size)
while the _remaining_ size is stored in the data.

https://github.com/runtimeverification/llvm-backend/blob/master/docs/binary_kore.md
-}
decodeMagicHeaderAndVersion :: Get (Version, Maybe Int)
decodeMagicHeaderAndVersion = do
    header <- getByteString 5
    unless (header == "\127KORE") $ fail "Invalid magic header for binary KORE"
    version <- Version <$> getInt16le <*> getInt16le <*> getInt16le
    unless (supported version) $
        fail $
            "Binary kore version " <> show version <> " not supported"
    (version,) <$> decodeLengthField version
  where
    -- read the length field if version >= 1.2, use zero (variable length) otherwise
    decodeLengthField :: Version -> Get (Maybe Int)
    decodeLengthField version
        | version >= Version 1 2 0 =
            (\n -> if n > 0 then Just (n + hdrSize) else Nothing) . fromIntegral <$> getWord64le
        | otherwise =
            pure Nothing
    hdrSize = 19

    supported :: Version -> Bool
    supported version = version.major == 1 && version.minor `elem` [0 .. 2]

decodeTerm' :: Maybe KoreDefinition -> Get Term
decodeTerm' mDef = do
    (version, mbSize) <- decodeMagicHeaderAndVersion
    runDecodeM version mDef $
        decodeBlock mbSize >>= \case
            [BTerm trmIdx] -> fmap stripRawTerm $ getTerm trmIdx
            _ -> fail "Expecting a single term on the top of the stack"

decodeTerm :: KoreDefinition -> Get Term
decodeTerm = decodeTerm' . Just

{- | Automatically transform `rawTerm(inj{SortX, KItem}(X))` to X:SortX
see https://github.com/runtimeverification/llvm-backend/issues/916
-}
stripRawTerm :: Term -> Term
stripRawTerm (SymbolApplication RawTermSymbol [] [Injection _ SortKItem t]) = t
stripRawTerm other = other

pattern RawTermSymbol :: Symbol
pattern RawTermSymbol =
    Symbol
        "rawTerm"
        []
        [SortKItem]
        SortKItem
        ( SymbolAttributes
                Constructor
                IsNotIdem
                IsNotAssoc
                IsNotMacroOrAlias
                CannotBeEvaluated
                Nothing
                Nothing
                Nothing
            )

decodePattern :: Maybe KoreDefinition -> Get Pattern
decodePattern mDef = do
    (version, mbSize) <- decodeMagicHeaderAndVersion
    runDecodeM version mDef $ do
        res <- reverse <$> decodeBlock mbSize
        case res of
            BTerm trmIdx : preds' -> do
                trm <- stripRawTerm <$> getTerm trmIdx
                preds <- forM preds' $ \case
                    BPredicate pIdx -> Predicate <$> getTerm pIdx
                    _ -> fail "Expecting a predicate"
                pure $ Pattern trm (Set.fromList preds) mempty
            _ -> fail "Expecting a term on the top of the stack"

encodeMagicHeaderAndVersion :: Version -> Put
encodeMagicHeaderAndVersion (Version major minor patch) = do
    putByteString "\127KORE"
    putInt16le major
    putInt16le minor
    putInt16le patch

encodeLength :: Int -> Put
encodeLength len =
    let chunk = fromIntegral $ len .&. 0x7f
        len' = len `shiftR` 7
     in if len > 0
            then do
                putWord8 (chunk .|. 0x80)
                encodeLength len'
            else putWord8 chunk

encodeString :: BS.ByteString -> Put
encodeString s = do
    putWord8 0x1
    encodeLength $ BS.length s
    putByteString s

encodeSort :: Sort -> Put
encodeSort = \case
    SortVar name -> do
        putWord8 KORESortVariable
        encodeString name
    SortApp name args -> do
        forM_ args encodeSort
        putWord8 KORECompositeSort
        encodeLength $ length args
        encodeString name

encodeTerm :: Term -> Put
encodeTerm = \case
    Var (Variable sort name) -> do
        encodeSort sort
        putWord8 KOREVariablePattern
        putWord8 KOREVariable
        encodeString name
    SymbolApplication sym sorts args -> encodeSymbolApplication sym.name sorts $ map Left args
    DomainValue sort value -> do
        putWord8 KOREStringPattern
        encodeString value
        encodeSymbol "\\dv" [sort]
        putWord8 KORECompositePattern
        encodeLength 1
    AndTerm t1 t2 -> encodeSymbolApplication "\\and" [sortOfTerm t1] [Left t1, Left t2]
    Injection source target t -> encodeSymbolApplication "inj" [source, target] [Left t]
    KMap def keyVals rest -> encodeTerm $ externaliseKmapUnsafe def keyVals rest
    KList def heads rest -> encodeTerm $ externaliseKList def heads rest
    KSet def heads rest -> encodeTerm $ externaliseKSet def heads rest

encodeSymbol :: ByteString -> [Sort] -> Put
encodeSymbol name sorts = do
    forM_ sorts encodeSort
    putWord8 KORESymbol
    encodeLength $ length sorts
    encodeString name

encodeSymbolApplication :: ByteString -> [Sort] -> [Either Term Predicate] -> Put
encodeSymbolApplication name sorts args = do
    forM_ args $ either encodeTerm encodePredicate
    encodeSymbol name sorts
    putWord8 KORECompositePattern
    encodeLength $ length args

encodePredicate :: Predicate -> Put
encodePredicate (Predicate t) =
    encodeSymbolApplication "\\equals" [] [Left t, Left TrueBool]

encodePattern :: Pattern -> Put
encodePattern Pattern{term, constraints} = do
    encodeTerm term
    forM_ constraints encodePredicate
