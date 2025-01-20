{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Booster.LLVM.Internal (
    API (..),
    KorePatternAPI (..),
    runLLVM,
    withDLib,
    mkAPI,
    ask,
    marshallTerm,
    marshallSort,
    -- testing only
    KoreStringPatternAPI (..),
    KoreSymbolAPI (..),
    KoreSortAPI (..),
    LlvmError (..),
) where

import Control.Exception (IOException)
import Control.Monad (foldM, forM_, void, (>=>))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, catch)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Reader qualified as Reader
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Foreign (ForeignPtr, finalizeForeignPtr, newForeignPtr, withForeignPtr)
import Foreign qualified
import Foreign.C qualified as C
import Foreign.C.Types (CSize (..))
import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import System.IO (hPutStrLn, stderr)
import System.Posix.DynamicLinker qualified as Linker

import Booster.LLVM.TH (dynamicBindings)
import Booster.Pattern.Base
import Booster.Pattern.Util (sortOfTerm)

data KorePattern
data KoreSort
data KoreSymbol
data KoreError
data Block
type SizeT = CSize

type KorePatternPtr = ForeignPtr KorePattern
type KoreSymbolPtr = ForeignPtr KoreSymbol
type KoreSortPtr = ForeignPtr KoreSort
type KoreErrorPtr = ForeignPtr KoreError

$(dynamicBindings "./cbits/kllvm-c.h")

newtype KoreStringPatternAPI = KoreStringPatternAPI
    { new :: ByteString -> IO KorePatternPtr
    }

newtype KoreTokenPatternAPI = KoreTokenPatternAPI
    { new :: ByteString -> KoreSortPtr -> IO KorePatternPtr
    }

data KoreSymbolAPI = KoreSymbolAPI
    { new :: ByteString -> IO KoreSymbolPtr
    , addArgument :: KoreSymbolPtr -> KoreSortPtr -> IO KoreSymbolPtr
    , cache :: IORef (HashMap (Symbol, [Sort]) KoreSymbolPtr)
    }

data KoreSortAPI = KoreSortAPI
    { new :: ByteString -> IO KoreSortPtr
    , addArgument :: KoreSortPtr -> KoreSortPtr -> IO KoreSortPtr
    , dump :: KoreSortPtr -> IO String
    , cache :: IORef (HashMap Sort KoreSortPtr)
    }

data KorePatternAPI = KorePatternAPI
    { new :: ByteString -> IO KorePatternPtr
    , addArgument :: KorePatternPtr -> KorePatternPtr -> IO KorePatternPtr
    , fromSymbol :: KoreSymbolPtr -> IO KorePatternPtr
    , string :: KoreStringPatternAPI
    , token :: KoreTokenPatternAPI
    , dump :: KorePatternPtr -> IO String
    }

-- data KoreErrorAPI = KoreErrorAPI
--     { new :: IO KoreErrorPtr
--     , isSuccess :: KoreErrorPtr -> IO Bool
--     , message :: KoreErrorPtr -> IO ByteString
--     }
newtype LlvmError = LlvmError ByteString deriving (Show, Eq, Data)

data API = API
    { patt :: KorePatternAPI
    , symbol :: KoreSymbolAPI
    , sort :: KoreSortAPI
    , simplifyBool :: KorePatternPtr -> IO (Either LlvmError Bool)
    , simplify :: KorePatternPtr -> KoreSortPtr -> IO (Either LlvmError ByteString)
    , collect :: IO ()
    }

newtype LLVM a = LLVM (ReaderT API IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

{- | Uses dlopen to load a .so/.dylib C library at runtime. For documentation of flags such as `RTL_LAZY`, consult e.g.
     https://man7.org/linux/man-pages/man3/dlopen.3.html
-}
withDLib :: FilePath -> (Linker.DL -> IO a) -> IO a
withDLib dlib = Linker.withDL dlib [Linker.RTLD_LAZY]

runLLVM :: API -> LLVM a -> IO a
runLLVM api (LLVM m) = runReaderT m api

mkAPI :: Linker.DL -> IO API
mkAPI dlib = flip runReaderT dlib $ do
    freePattern <- {-# SCC "LLVM.pattern.free" #-} korePatternFreeFunPtr

    newCompositePattern <- koreCompositePatternNew
    let newPattern name =
            {-# SCC "LLVM.pattern.new" #-}
            BS.useAsCString name $
                newCompositePattern
                    >=> newForeignPtr freePattern

    addArgumentCompositePattern <- koreCompositePatternAddArgument
    let addArgumentPattern parent child =
            {-# SCC "LLVM.pattern.addArgument" #-}
            do
                withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentCompositePattern rawParent
                finalizeForeignPtr child
                pure parent

    newString <- koreStringPatternNewWithLen
    let string = KoreStringPatternAPI $ \name ->
            {-# SCC "LLVM.pattern.string" #-}
            BS.useAsCStringLen name $ \(rawStr, len) ->
                newString rawStr (fromIntegral len)
                    >>= newForeignPtr freePattern

    newToken <- korePatternNewTokenWithLen
    let token = KoreTokenPatternAPI $ \name sort ->
            {-# SCC "LLVM.pattern.token" #-}
            BS.useAsCStringLen name $ \(rawName, len) ->
                withForeignPtr sort $
                    newToken rawName (fromIntegral len)
                        >=> newForeignPtr freePattern

    compositePatternFromSymbol <- koreCompositePatternFromSymbol
    let fromSymbol sym =
            {-# SCC "LLVM.pattern.fromSymbol" #-}
            withForeignPtr sym $
                compositePatternFromSymbol
                    >=> newForeignPtr freePattern

    dumpPattern' <- korePatternDump
    let dumpPattern ptr =
            {-# SCC "LLVM.pattern.dump" #-}
            withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dumpPattern' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str

    let patt =
            KorePatternAPI
                { new = newPattern
                , addArgument = addArgumentPattern
                , string
                , token
                , fromSymbol
                , dump = dumpPattern
                }

    freeSymbol <- {-# SCC "LLVM.symbol.free" #-} koreSymbolFreeFunPtr

    newSymbol' <- koreSymbolNew
    let newSymbol name =
            {-# SCC "LLVM.symbol.new" #-}
            liftIO $
                BS.useAsCString name $
                    newSymbol'
                        >=> newForeignPtr freeSymbol

    addArgumentSymbol' <- koreSymbolAddFormalArgument
    let addArgumentSymbol sym sort =
            {-# SCC "LLVM.symbol.addArgument" #-}
            do
                withForeignPtr sym $ \rawSym -> withForeignPtr sort $ addArgumentSymbol' rawSym
                pure sym

    symbolCache <- liftIO $ newIORef mempty

    let symbol = KoreSymbolAPI{new = newSymbol, addArgument = addArgumentSymbol, cache = symbolCache}

    freeSort <- {-# SCC "LLVM.sort.free" #-} koreSortFreeFunPtr

    newSort' <- koreCompositeSortNew
    let newSort name =
            {-# SCC "LLVM.sort.new" #-}
            BS.useAsCString name $
                newSort'
                    >=> newForeignPtr freeSort

    addArgumentSort' <- koreCompositeSortAddArgument
    let addArgumentSort parent child =
            {-# SCC "LLVM.sort.addArgument" #-}
            do
                withForeignPtr parent $ \rawParent -> withForeignPtr child $ addArgumentSort' rawParent
                pure parent

    dumpSort' <- koreSortDump
    let dumpSort ptr =
            {-# SCC "LLVM.sort.dump" #-}
            liftIO $ withForeignPtr ptr $ \rawPtr -> do
                strPtr <- dumpSort' rawPtr
                str <- C.peekCString strPtr
                Foreign.free strPtr
                pure str

    sortCache <- liftIO $ newIORef mempty

    let sort = KoreSortAPI{new = newSort, addArgument = addArgumentSort, dump = dumpSort, cache = sortCache}

    freeError <- {-# SCC "LLVM.error.free" #-} koreErrorFreeFunPtr
    newError <- {-# SCC "LLVM.error.new" #-} (>>= newForeignPtr freeError) <$> koreErrorNew
    isSuccess <- {-# SCC "LLVM.error.isSuccess" #-} (>=> pure . (== 1)) <$> koreErrorIsSuccess
    errorMessage <- {-# SCC "LLVM.error.message" #-} (>=> BS.packCString) <$> koreErrorMessage

    initialize <- kllvmInit
    liftIO initialize
    collect <- kllvmFreeAllMemory

    simplifyBool' <- koreSimplifyBool
    let simplifyBool p =
            {-# SCC "LLVM.simplifyBool" #-}
            do
                err <- newError
                withForeignPtr err $ \errPtr ->
                    withForeignPtr p $ \pPtr -> do
                        res <- simplifyBool' errPtr pPtr
                        success <- isSuccess errPtr
                        if success
                            then pure $ Right $ res == 1
                            else do
                                Left . LlvmError <$> errorMessage errPtr

    simplify' <- koreSimplify
    let simplify pat srt =
            {-# SCC "LLVM.simplify" #-}
            liftIO $ do
                err <- newError
                withForeignPtr err $ \errPtr ->
                    withForeignPtr pat $ \patPtr ->
                        withForeignPtr srt $ \sortPtr ->
                            alloca $ \lenPtr ->
                                alloca $ \strPtr -> do
                                    simplify' errPtr patPtr sortPtr strPtr lenPtr
                                    success <- isSuccess errPtr
                                    if success
                                        then do
                                            len <- fromIntegral <$> peek lenPtr
                                            cstr <- peek strPtr
                                            result <- BS.packCStringLen (cstr, len)
                                            Foreign.free cstr
                                            pure $ Right result
                                        else Left . LlvmError <$> errorMessage errPtr

    mutableBytesEnabled <-
        kllvmMutableBytesEnabled `catch` \(_ :: IOException) -> pure (pure 0)
    liftIO $
        whenM ((/= 0) <$> mutableBytesEnabled) $
            hPutStrLn
                stderr
                "[Warn] Using an LLVM backend compiled with --llvm-mutable-bytes (unsound byte array semantics)"

    pure API{patt, symbol, sort, simplifyBool, simplify, collect}

ask :: LLVM API
ask = LLVM Reader.ask

marshallSymbol :: Symbol -> [Sort] -> LLVM KoreSymbolPtr
marshallSymbol sym sorts = do
    kore <- ask
    cache <- liftIO $ readIORef kore.symbol.cache
    case HM.lookup (sym, sorts) cache of
        Just ptr -> pure ptr
        Nothing -> do
            sym' <- liftIO $ kore.symbol.new sym.name
            liftIO $ modifyIORef' kore.symbol.cache $ HM.insert (sym, sorts) sym'
            foldM (\symbol sort -> marshallSort sort >>= liftIO . kore.symbol.addArgument symbol) sym' sorts

marshallSort :: Sort -> LLVM KoreSortPtr
marshallSort = \case
    s@(SortApp name args) -> do
        kore <- ask
        cache <- liftIO $ readIORef kore.sort.cache
        case HM.lookup s cache of
            Just ptr -> pure ptr
            Nothing -> do
                sort <- liftIO $ kore.sort.new name
                forM_ args $ marshallSort >=> liftIO . kore.sort.addArgument sort
                liftIO $ modifyIORef' kore.sort.cache $ HM.insert s sort
                pure sort
    SortVar varName -> error $ "marshalling SortVar " <> show varName <> " unsupported"

marshallTerm :: Term -> LLVM KorePatternPtr
marshallTerm t = do
    kore <- ask
    case t of
        SymbolApplication symbol sorts trms -> do
            trm <- liftIO . kore.patt.fromSymbol =<< marshallSymbol symbol sorts
            forM_ trms $ marshallTerm >=> liftIO . kore.patt.addArgument trm
            pure trm
        AndTerm l r -> do
            andSym <- liftIO $ kore.symbol.new "\\and"
            void $ liftIO . kore.symbol.addArgument andSym =<< marshallSort (sortOfTerm l)
            trm <- liftIO $ kore.patt.fromSymbol andSym
            void $ liftIO . kore.patt.addArgument trm =<< marshallTerm l
            liftIO . kore.patt.addArgument trm =<< marshallTerm r
        DomainValue sort val ->
            marshallSort sort >>= liftIO . kore.patt.token.new val
        Var varName -> error $ "marshalling Var " <> show varName <> " unsupported"
        Injection source target trm -> do
            inj <- liftIO . kore.patt.fromSymbol =<< marshallSymbol injectionSymbol [source, target]
            marshallTerm trm >>= liftIO . kore.patt.addArgument inj
        KMap def keyVals rest -> marshallTerm $ externaliseKmapUnsafe def keyVals rest
        KList def heads rest -> marshallTerm $ externaliseKList def heads rest
        KSet def heads rest -> marshallTerm $ externaliseKSet def heads rest
