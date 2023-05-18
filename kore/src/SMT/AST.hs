{- |
Module      : SMT.AST
Description : AST for (parts of) the SMT commands
Copyright   : (c) Iavor S. Diatchki, 2014
              (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com

Use the Smt* versions of data types to work with the SimpleSMT interface.
-}
module SMT.AST (
    Constructor (..),
    ConstructorArgument (..),
    DataTypeDeclaration (..),
    FunctionDeclaration (..),
    SortDeclaration (..),
    SExpr (..),
    buildSExpr,
    parseSExpr,
    parseSExprFile,
    readSExpr,
    readSExprs,
    sendSExpr,
    showSExpr,
    buildText,
    mapSExpr,
    SmtConstructor,
    SmtConstructorArgument,
    SmtDataTypeDeclaration,
    SmtSortDeclaration,
    SmtFunctionDeclaration,
) where

import Data.Binary (Binary)
import Data.Char (
    isSpace,
 )
import Data.String (
    IsString (..),
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Internal.Builder (
    Builder,
 )
import Data.Text.Internal.Builder qualified as Text.Builder
import Data.Text.Lazy qualified as Text.Lazy
import Data.Void (
    Void,
 )
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Prelude.Kore
import System.IO (
    Handle,
    hPutChar,
 )
import Text.Megaparsec (
    Parsec,
 )
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char qualified as Parser
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | S-expressions, the basic format for SMT-LIB 2.
data SExpr
    = Atom !Text
    | List ![SExpr]
    deriving stock (GHC.Generic, Eq, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance NFData SExpr

instance IsString SExpr where
    fromString = Atom . Text.pack

{- | An argument to a data type constructor.

The name can be used as a getter in smt solvers. (Note: this is currently not
working due to a bug in z3 data type declaration,
see https://github.com/Z3Prover/z3/issues/2217 , also see the similar comment
in declareDatatypes in SimpleSMT.hs)
-
-}
data ConstructorArgument sort name = ConstructorArgument
    { name :: !name
    , argType :: !sort
    }
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

{- | A data type constructor.
-
-}
data Constructor sort symbol name = Constructor
    { name :: !symbol
    , arguments :: ![ConstructorArgument sort name]
    }
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

{- | A constructor-based data type declaration.

If the list of constructors is empty, the data type is empty.
-
-}
data DataTypeDeclaration sort symbol name = DataTypeDeclaration
    { name :: !name
    , typeArguments :: ![name]
    , constructors :: ![Constructor sort symbol name]
    }
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance
    (Debug sort, Debug symbol, Debug name) =>
    Debug (DataTypeDeclaration sort symbol name)

instance
    (Debug sort, Debug symbol, Debug name, Diff sort, Diff symbol, Diff name) =>
    Diff (DataTypeDeclaration sort symbol name)

-- | A non-constructor-based data type declaration.
data SortDeclaration name = SortDeclaration
    { name :: !name
    , arity :: Int
    }
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | A function declaration.
data FunctionDeclaration sort name = FunctionDeclaration
    { name :: !name
    , inputSorts :: ![sort]
    , resultSort :: !sort
    }
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (Debug sort, Debug name) => Debug (FunctionDeclaration sort name)

instance
    (Debug sort, Debug name, Diff sort, Diff name) =>
    Diff (FunctionDeclaration sort name)

-- | Instantiate Constructor with the types needed by SimpleSMT.
type SmtConstructor = Constructor SExpr Text SExpr

-- | Instantiate ConstructorArgument with the types needed by SimpleSMT.
type SmtConstructorArgument = ConstructorArgument SExpr SExpr

-- | Instantiate DataTypeDeclaration with the types needed by SimpleSMT.
type SmtDataTypeDeclaration = DataTypeDeclaration SExpr Text SExpr

-- | Instantiate SortDeclaration with the types needed by SimpleSMT.
type SmtSortDeclaration = SortDeclaration SExpr

-- | Instantiate FunctionDeclaration with the types needed by SimpleSMT.
type SmtFunctionDeclaration = FunctionDeclaration SExpr SExpr

-- | Stream an S-expression into 'Builder'.
buildSExpr :: SExpr -> Builder
buildSExpr =
    \case
        Atom x -> Text.Builder.fromText x
        List es ->
            Text.Builder.singleton '('
                <> foldMap (\e -> buildSExpr e <> Text.Builder.singleton ' ') es
                <> Text.Builder.singleton ')'

-- | Show an S-expression.
showSExpr :: SExpr -> String
showSExpr = Text.Lazy.unpack . Text.Builder.toLazyText . buildSExpr

{- | Send an S-expression directly to a 'Handle'.

@sendSExpr@ performs slightly better than @buildSExpr@ by avoiding almost all
intermediate allocation.

@sendSExpr@ sends only the S-expression; it does not send the trailing newline
which signals the end of a command.
-}
sendSExpr :: Handle -> SExpr -> IO ()
sendSExpr h = sendSExprWorker
  where
    sendSExprWorker =
        \case
            Atom atom -> Text.hPutStr h atom
            List atoms -> do
                hPutChar h '('
                mapM_ sendListElement atoms
                hPutChar h ')'
    sendListElement sExpr = do
        sendSExprWorker sExpr
        hPutChar h ' '

type Parser = Parsec Void Text

parseSExprSpace :: Parser ()
parseSExprSpace = Lexer.space Parser.space1 skipLineComment empty
{-# INLINE parseSExprSpace #-}

skipLineComment :: Parser ()
skipLineComment = Lexer.skipLineComment ";"
{-# INLINE skipLineComment #-}

-- | Basic S-expression parser.
parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> parseList
  where
    parseAtom :: Parser SExpr
    parseAtom = lexeme (Atom <$> Parser.takeWhile1P Nothing notSpecial)

    parseList :: Parser SExpr
    parseList =
        List <$> (lparen *> Parser.many parseSExpr <* rparen)

    special :: Char -> Bool
    special c = isSpace c || c == '(' || c == ')' || c == ';'

    notSpecial :: Char -> Bool
    notSpecial = not . special

    lparen :: Parser Char
    lparen = lexeme (Parser.char '(')

    rparen :: Parser Char
    rparen = lexeme (Parser.char ')')

    lexeme :: Parser a -> Parser a
    lexeme = Lexer.lexeme parseSExprSpace

parseSExprFile :: Parser [SExpr]
parseSExprFile = parseSExprSpace *> Parser.some parseSExpr

-- | Parse one S-expression.
readSExpr :: MonadFail m => Text -> m SExpr
readSExpr txt =
    case Parser.parse parseSExpr "<unknown>" txt of
        Left err -> fail (Parser.errorBundlePretty err)
        Right sExpr -> return sExpr

-- | Parse many S-expressions.
readSExprs :: Text -> [SExpr]
readSExprs txt =
    fromMaybe
        []
        (Parser.parseMaybe (Parser.some parseSExpr) txt)

buildText :: SExpr -> Text
buildText = Text.Lazy.toStrict . Text.Builder.toLazyText . buildSExpr

mapSExpr :: (Text -> Text) -> SExpr -> SExpr
mapSExpr f (Atom text) = Atom (f text)
mapSExpr f (List sExprs) = List (mapSExpr f <$> sExprs)
