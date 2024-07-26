{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.JsonRpc.Utils (
    diffJson,
    DiffResult (..),
    isIdentical,
    renderResult,
    KoreRpcJson (..),
    decodeKoreRpc,
    KoreRpcType (..),
    rpcTypeOf,
    typeString,
    diffBy,
    methodOfRpcCall,
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Data.Aeson as Json
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy.Char8 qualified as BS

import Data.Maybe (fromMaybe)
import Network.JSONRPC
import Prettyprinter
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Extra (withTempDir)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

import Booster.Definition.Base qualified as Internal
import Booster.Pattern.Pretty
import Booster.Prettyprinter
import Booster.Syntax.Json.Internalise
import Data.Binary.Builder (fromLazyByteString, toLazyByteString)
import Data.List (intersperse)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text
import Kore.JsonRpc.Error qualified as RpcError
import Kore.JsonRpc.Types
import Kore.Syntax.Json.Types hiding (Left, Right)
import Prettyprinter qualified as Pretty

diffJson :: BS.ByteString -> BS.ByteString -> DiffResult
diffJson file1 file2 =
    case (decodeKoreRpc file1, decodeKoreRpc file2) of
        -- special case for Branching execute result with two next states,
        -- useful when comparing responses of `kore-rpc` and `kore-rpc-booster`
        (contents1@(RpcResponse (Execute res1)), RpcResponse (Execute res2))
            | sameModuloBranchOrder res1 res2 -> Identical $ rpcTypeOf contents1
        (contents1, contents2)
            | contents1 == contents2 ->
                Identical $ rpcTypeOf contents1
        (NotRpcJson lines1, NotRpcJson lines2) -> do
            TextDiff (BS.unlines lines1) (BS.unlines lines2)
        (other1, other2)
            | rpcTypeOf other1 /= rpcTypeOf other2 ->
                DifferentType (rpcTypeOf other1) (rpcTypeOf other2)
            | otherwise -> do
                JsonDiff
                    (rpcTypeOf other1)
                    (encodePretty' rpcJsonConfig other1)
                    (encodePretty' rpcJsonConfig other2)
  where
    -- \| Branching execution results are considered equivalent if
    -- \* they both have two branches
    -- \* branches are syntactically the same, but may be in different order
    sameModuloBranchOrder :: ExecuteResult -> ExecuteResult -> Bool
    sameModuloBranchOrder res1 res2
        | res1.reason == Branching && res1.reason == res2.reason =
            case (res1.nextStates, res2.nextStates) of
                (Just xs, Just ys) ->
                    length xs == 2 && length ys == 2 && (xs == ys || xs == reverse ys)
                _ -> False
        | otherwise = False

data DiffResult
    = Identical KoreRpcType
    | DifferentType KoreRpcType KoreRpcType
    | JsonDiff KoreRpcType BS.ByteString BS.ByteString
    | TextDiff BS.ByteString BS.ByteString
    deriving (Eq, Show)

isIdentical :: DiffResult -> Bool
isIdentical Identical{} = True
isIdentical _ = False

renderResult :: FilePath -> FilePath -> DiffResult -> BS.ByteString
renderResult korefile1 korefile2 = \case
    Identical rpcType ->
        BS.unwords
            ["Files", file1, "and", file2, "are identical", typeString rpcType <> "s"]
    DifferentType type1 type2 ->
        BS.unlines
            [ "Json data in files is of different type"
            , "  * File " <> file1 <> ": " <> typeString type1
            , "  * File " <> file2 <> ": " <> typeString type2
            ]
    JsonDiff rpcType first second ->
        BS.unlines
            [ BS.unwords ["Files", file1, "and", file2, "are different", typeString rpcType <> "s"]
            , BS.pack $ fromMaybe (error "Expected difference") $ renderDiff first second
            ]
    TextDiff first second ->
        BS.unlines
            [ BS.unwords ["Files", file1, "and", file2, "are different non-json files"]
            , BS.pack $ fromMaybe (error "Expected difference") $ renderDiff first second
            ]
  where
    file1 = BS.pack korefile1
    file2 = BS.pack korefile2

decodeKoreRpc :: BS.ByteString -> KoreRpcJson
decodeKoreRpc input =
    fromMaybe (NotRpcJson splitInput) $
        try [rpcRequest, rpcResponse, rpcError, koreJson, unknown]
  where
    try = foldl1 (<|>)
    rpcRequest = fmap RpcRequest $ do
        req <- Json.decode @Request input
        parser <- parseParams req.getReqMethod
        parseMaybe parser req.getReqParams
    rpcResponse = do
        resp <- Json.decode @Response input
        case resp of
            ResponseError{} -> extractError resp.getError
            OrphanError{} -> extractError resp.getError
            Response{} ->
                fmap RpcResponse . try $
                    [ Execute <$> parseMaybe (Json.parseJSON @ExecuteResult) resp.getResult
                    , Implies <$> parseMaybe (Json.parseJSON @ImpliesResult) resp.getResult
                    , Simplify <$> parseMaybe (Json.parseJSON @SimplifyResult) resp.getResult
                    , AddModule <$> parseMaybe (Json.parseJSON @AddModuleResult) resp.getResult
                    , GetModel <$> parseMaybe (Json.parseJSON @GetModelResult) resp.getResult
                    ]
    rpcError =
        Json.decode @ErrorObj input >>= extractError
    extractError = \case
        ErrorObj msg code mbData ->
            pure $ RpcError msg code mbData
        ErrorVal{} -> fail "arbitrary json can be an ErrorVal"
    koreJson =
        RpcKoreJson <$> Json.decode @KoreJson input
    unknown =
        RpcJson <$> Json.decode @Object input
    -- last resort: break the bytestring into lines at json-relevant
    -- characters (ignoring quoting)
    splitInput = BS.splitWith (`elem` [':', ',', '{', '}']) input

-- | helper type enumerating all Kore-RPC json requests and responses
data KoreRpcJson
    = RpcRequest (API 'Req)
    | RpcResponse (API 'Res)
    | RpcError String Int Value
    | RpcKoreJson KoreJson
    | RpcJson Object
    | NotRpcJson [BS.ByteString]
    deriving stock (Eq, Show)

instance ToJSON KoreRpcJson where
    toJSON = \case
        RpcRequest req ->
            case req of -- missing instance ToJSON (API 'Req), inlined
                Execute r -> toJSON r
                Implies r -> toJSON r
                Simplify r -> toJSON r
                AddModule r -> toJSON r
                GetModel r -> toJSON r
                Cancel -> toJSON ()
        RpcResponse r -> toJSON r
        RpcError msg code v -> toJSON (msg, code, v)
        RpcKoreJson t -> toJSON t
        RpcJson v -> toJSON v
        NotRpcJson bs -> toJSON $ map BS.unpack bs

-- | Information about the kind of object in 'KoreRpcJson' (without payload)
data KoreRpcType
    = RpcReq APIMethod
    | RpcResp APIMethod
    | RpcErr
    | RpcKore
    | RpcJs
    | NotRpcJs
    deriving (Eq, Ord)

instance Show KoreRpcType where
    show = \case
        RpcReq method -> show method <> " request"
        RpcResp method -> show method <> " response"
        RpcErr -> "error response"
        RpcKore -> "Kore term"
        RpcJs -> "unknown object"
        NotRpcJs -> "non-JSON file"

typeString :: KoreRpcType -> BS.ByteString
typeString = BS.pack . show

rpcTypeOf :: KoreRpcJson -> KoreRpcType
rpcTypeOf = \case
    RpcRequest req -> RpcReq $ methodOfRpcCall req
    RpcResponse r -> RpcResp $ methodOfRpcCall r
    RpcError{} -> RpcErr
    RpcKoreJson{} -> RpcKore
    RpcJson{} -> RpcJs
    NotRpcJson{} -> NotRpcJs

methodOfRpcCall :: API r -> APIMethod
methodOfRpcCall = \case
    Execute _ -> ExecuteM
    Implies _ -> ImpliesM
    Simplify _ -> SimplifyM
    AddModule _ -> AddModuleM
    GetModel _ -> GetModelM
    Cancel -> error "Cancel"

-------------------------------------------------------------------
-- doing the actual diff when output is requested

renderDiff :: BS.ByteString -> BS.ByteString -> Maybe String
renderDiff first second
    | first == second = Nothing
    | otherwise = renderDiff' ["-w"] first second

renderDiff' :: [String] -> BS.ByteString -> BS.ByteString -> Maybe String
renderDiff' diffOptions first second = unsafePerformIO . withTempDir $ \dir -> do
    let path1 = dir </> "diff_file1.txt"
        path2 = dir </> "diff_file2.txt"
    BS.writeFile path1 first
    BS.writeFile path2 second
    (result, str, _) <- readProcessWithExitCode "diff" (diffOptions <> [path1, path2]) ""
    case result of
        ExitSuccess -> pure Nothing
        ExitFailure 1 -> pure $ Just str
        ExitFailure n -> error $ "diff process exited with code " <> show n

-------------------------------------------------------------------
-- compute diff on internalised patterns (to normalise collections etc)
-- This uses the `pretty` instance for a textual diff.
diffBy :: Internal.KoreDefinition -> KorePattern -> KorePattern -> Maybe String
diffBy def pat1 pat2 =
    renderDiff' ["-c", "-w"] (internalise pat1) (internalise pat2)
  where
    renderBS :: TermOrPredicates -> BS.ByteString
    renderBS (Predicates ps) =
        ( BS.pack . renderDefault . Pretty.vsep $
            concat
                [ "Conditions:"
                    : fmap
                        (Pretty.indent 4 . pretty . PrettyWithModifiers @['Decoded, 'Truncated])
                        (Set.toList ps.boolPredicates)
                , "Ceil conditions:"
                    : map
                        (Pretty.indent 4 . pretty . PrettyWithModifiers @['Decoded, 'Truncated])
                        (Set.toList ps.ceilPredicates)
                , "Substitutions:"
                    : fmap
                        (Pretty.indent 4)
                        ( map
                            ( \(k, v) ->
                                pretty (PrettyWithModifiers @['Decoded, 'Truncated] k)
                                    <+> "="
                                    <+> pretty (PrettyWithModifiers @['Decoded, 'Truncated] v)
                            )
                            (Map.toList ps.substitution)
                        )
                ]
        )
            <> if null ps.unsupported
                then ""
                else BS.unlines ("Unsupported parts:" : map Json.encode ps.unsupported)
    renderBS (TermAndPredicates p m u) =
        ( BS.pack . renderDefault $
            pretty (PrettyWithModifiers @['Decoded, 'Truncated] p)
                <+> vsep
                    ( map
                        ( \(k, v) ->
                            pretty (PrettyWithModifiers @['Decoded, 'Truncated] k)
                                <+> "="
                                <+> pretty (PrettyWithModifiers @['Decoded, 'Truncated] v)
                        )
                        (Map.toList m)
                    )
        )
            <> if null u then "" else BS.unlines ("Unsupported parts: " : map Json.encode u)
    internalise =
        either
            ( ("Pattern could not be internalised: " <>)
                . toLazyByteString
                . prettyRpcErrors
                . map patternErrorToRpcError
            )
            renderBS
            . runExcept
            . internaliseTermOrPredicate DisallowAlias IgnoreSubsorts Nothing def
    prettyRpcErrors = \case
        [] -> "unknown error"
        [e] -> prettyRpcError e
        (e : es) -> prettyRpcError e <> "\n" <> prettyRpcErrors es

    prettyRpcError RpcError.ErrorWithTermAndContext{error = err, term, context} =
        Text.encodeUtf8Builder err
            <> "\n"
            <> maybe "" ((<> "\n") . fromLazyByteString . Json.encode) term
            <> maybe "" (mconcat . intersperse ", " . map Text.encodeUtf8Builder) context
