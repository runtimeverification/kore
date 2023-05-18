{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Builtin.Verifiers (
    Verifiers (..),
    SymbolVerifier (..),
    SymbolVerifiers,
    SortDeclVerifier,
    SortDeclVerifiers,
    SortVerifier (..),
    ApplicationVerifier (..),
    SymbolKey (..),
    ApplicationVerifiers,
    lookupApplicationVerifier,
    PatternVerifierHook (..),
    PatternVerifier,
    domainValuePatternVerifierHook,
    applicationPatternVerifierHooks,
    Parser,
    symbolVerifier,
    sortDeclVerifier,

    -- * Declaring builtin verifiers
    verifySortDecl,
    getUnitId,
    getElementId,
    getConcatId,
    assertSymbolHook,
    assertSymbolResultSort,
    verifySort,
    verifySortHasDomainValues,
    expectDomainValue,
    acceptAnySort,
    verifySymbol,
    verifySymbolArguments,
    parseString,
) where

import Control.Error (
    MaybeT (..),
 )
import Control.Monad (
    zipWithM_,
 )
import Control.Monad qualified as Monad
import Data.Binary (Binary)
import Data.Functor.Foldable qualified as Recursive
import Data.HashMap.Strict (
    HashMap,
 )
import Data.HashMap.Strict qualified as HashMap
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Data.Void (
    Void,
 )
import GHC.Generics qualified as GHC
import Kore.AST.Error qualified as Kore.Error
import Kore.Attribute.Attributes (
    Attributes (..),
 )
import Kore.Attribute.Hook (
    Hook (..),
 )
import Kore.Attribute.Parser qualified as Attribute.Parser
import Kore.Attribute.Sort qualified as Attribute
import Kore.Attribute.Sort.Concat qualified as Attribute.Sort
import Kore.Attribute.Sort.Element qualified as Attribute.Sort
import Kore.Attribute.Sort.HasDomainValues qualified as Attribute
import Kore.Attribute.Sort.Unit qualified as Attribute.Sort
import Kore.Attribute.Symbol qualified as Attribute (
    Symbol (..),
 )
import Kore.Attribute.Symbol qualified as Attribute.Symbol
import Kore.Attribute.Synthetic (
    synthesize,
 )
import Kore.Builtin.Error
import Kore.Error (
    Error,
 )
import Kore.Error qualified
import Kore.IndexedModule.IndexedModule (
    IndexedModuleSyntax,
    VerifiedModule,
 )
import Kore.IndexedModule.Resolvers qualified as IndexedModule
import Kore.Internal.TermLike as TermLike
import Kore.Syntax.Definition (
    ParsedSentenceSort,
    ParsedSentenceSymbol,
    SentenceSort (..),
    SentenceSymbol (..),
 )
import Kore.Unparser
import Kore.Validate.AttributesVerifier qualified as Verifier.Attributes
import Kore.Validate.Error (
    VerifyError,
 )
import Kore.Validate.PatternVerifier.PatternVerifier (
    PatternVerifier,
    PatternVerifierHook (..),
 )
import Kore.Validate.PatternVerifier.PatternVerifier qualified as PatternVerifier
import Kore.Verified qualified as Verified
import Prelude.Kore
import Text.Megaparsec (
    Parsec,
 )
import Text.Megaparsec qualified as Parsec

type Parser = Parsec Void Text

-- | Verify a sort declaration.
type SortDeclVerifier =
    -- | Indexed module, to look up sort declarations
    VerifiedModule Attribute.Symbol ->
    -- | Sort declaration to verify
    ParsedSentenceSort ->
    -- | Declared sort attributes
    Attribute.Sort ->
    Either (Error VerifyError) ()

newtype SortVerifier = SortVerifier
    { runSortVerifier ::
        (Id -> Either (Error VerifyError) SentenceSort) ->
        Sort ->
        Either (Error VerifyError) ()
    }

-- | @SortDeclVerifiers@ associates a sort verifier with its builtin sort name.
type SortDeclVerifiers = HashMap Text SortDeclVerifier

newtype SymbolVerifier = SymbolVerifier
    { runSymbolVerifier ::
        (Id -> Either (Error VerifyError) SentenceSort) ->
        ParsedSentenceSymbol ->
        Either (Error VerifyError) ()
    }

{- | @SymbolVerifiers@ associates a @SymbolVerifier@ with each builtin
  symbol name.
-}
type SymbolVerifiers = HashMap Text SymbolVerifier

-- | Verify (and internalize) an application pattern.
newtype ApplicationVerifier patternType = ApplicationVerifier
    { runApplicationVerifier ::
        Application Symbol Verified.Pattern ->
        PatternVerifier (TermLikeF VariableName Verified.Pattern)
    }

-- | @SymbolKey@ names builtin functions and constructors.
data SymbolKey
    = -- | A builtin function identified by its @hook@ attribute.
      HookedSymbolKey !Text
    | -- | A builtin constructor identified by its @klabel@ attribute.
      KlabelSymbolKey !Text
    deriving stock (Eq, Ord)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)

instance Hashable SymbolKey

type ApplicationVerifiers patternType =
    HashMap SymbolKey (ApplicationVerifier patternType)

lookupApplicationVerifier ::
    Symbol ->
    ApplicationVerifiers patternType ->
    Maybe (ApplicationVerifier patternType)
lookupApplicationVerifier symbol verifiers = do
    key <- getHook symbol <|> getKlabel symbol
    HashMap.lookup key verifiers
  where
    getHook =
        fmap HookedSymbolKey
            . Attribute.Symbol.getHook
            . Attribute.Symbol.hook
            . symbolAttributes
    getKlabel =
        fmap KlabelSymbolKey
            . Attribute.Symbol.getKlabel
            . Attribute.Symbol.klabel
            . symbolAttributes

applicationPatternVerifierHooks ::
    ApplicationVerifiers patternType ->
    PatternVerifierHook
applicationPatternVerifierHooks applicationVerifiers =
    PatternVerifierHook $ \termLike ->
        let _ :< termLikeF = Recursive.project termLike
         in case termLikeF of
                ApplySymbolF application
                    | Just verifier <- lookupVerifier symbol ->
                        synthesize <$> runApplicationVerifier verifier application
                  where
                    Application{applicationSymbolOrAlias = symbol} = application
                _ -> return termLike
  where
    lookupVerifier symbol =
        lookupApplicationVerifier symbol applicationVerifiers

domainValuePatternVerifierHook ::
    Text ->
    ( DomainValue Sort Verified.Pattern ->
      PatternVerifier (TermLikeF VariableName Verified.Pattern)
    ) ->
    PatternVerifierHook
domainValuePatternVerifierHook hook verifier =
    PatternVerifierHook $ \termLike ->
        let _ :< termLikeF = Recursive.project termLike
         in case termLikeF of
                DomainValueF domainValue
                    | SortActualSort _ <- domainValueSort -> do
                        hook' <- lookupHookSort domainValueSort
                        fromMaybe (return termLike) $ do
                            Monad.guard (hook' == Just hook)
                            let context =
                                    unwords
                                        [ "Verifying builtin sort"
                                        , "'" ++ Text.unpack hook ++ "'"
                                        ]
                                verify =
                                    Kore.Error.withContext context
                                        . Kore.Error.withContext
                                            "While parsing domain value"
                                        . fmap synthesize
                                        . verifier
                            pure (verify domainValue)
                  where
                    DomainValue{domainValueSort} = domainValue
                _ -> return termLike

-- | Verify builtin sorts, symbols, and patterns.
data Verifiers = Verifiers
    { sortDeclVerifiers :: SortDeclVerifiers
    , symbolVerifiers :: SymbolVerifiers
    , patternVerifierHook :: PatternVerifierHook
    }

instance Semigroup Verifiers where
    (<>) a b =
        Verifiers
            { sortDeclVerifiers = on (<>) sortDeclVerifiers a b
            , symbolVerifiers = on (<>) symbolVerifiers a b
            , patternVerifierHook = on (<>) patternVerifierHook a b
            }

instance Monoid Verifiers where
    mempty =
        Verifiers
            { sortDeclVerifiers = mempty
            , symbolVerifiers = mempty
            , patternVerifierHook = mempty
            }

{- | Look up and apply a builtin sort declaration verifier.

The 'Hook' name should refer to a builtin sort; if it is unset or the name is
not recognized, verification succeeds.
-}
sortDeclVerifier :: Verifiers -> Hook -> SortDeclVerifier
sortDeclVerifier Verifiers{sortDeclVerifiers} hook =
    let hookedSortVerifier = do
            -- Get the builtin sort name.
            sortName <- getHook hook
            HashMap.lookup sortName sortDeclVerifiers
     in fromMaybe
            -- There is nothing to verify because either
            -- 1. the sort is not hooked, or
            -- 2. there is no SortVerifier registered to the hooked name.
            -- In either case, there is nothing more to do.
            (\_ _ _ -> pure ())
            -- Invoke the verifier that is registered to this builtin sort.
            hookedSortVerifier

{- | Look up and apply a builtin symbol verifier.

The 'Hook' name should refer to a builtin symbol; if it is unset or the name is
not recognized, verification succeeds.
-}
symbolVerifier :: Verifiers -> Hook -> SymbolVerifier
symbolVerifier Verifiers{symbolVerifiers} hook =
    let hookedSymbolVerifier = do
            -- Get the builtin sort name.
            symbolName <- getHook hook
            HashMap.lookup symbolName symbolVerifiers
     in fromMaybe
            -- There is nothing to verify because either
            -- 1. the symbol is not hooked, or
            -- 2. there is no SymbolVerifier registered to the hooked name.
            -- In either case, there is nothing more to do.
            (SymbolVerifier $ \_ _ -> pure ())
            -- Invoke the verifier that is registered to this builtin symbol.
            hookedSymbolVerifier

{- | Verify a builtin sort declaration.

  Check that the hooked sort does not take any sort parameters.
-}
verifySortDecl :: SortDeclVerifier
verifySortDecl _ SentenceSort{sentenceSortParameters} _ =
    getZeroParams sentenceSortParameters

-- | Throw a 'VerifyError' if there are any sort parameters.
getZeroParams :: [SortVariable] -> Either (Error VerifyError) ()
getZeroParams =
    \case
        [] -> return ()
        params ->
            Kore.Error.koreFail
                ("Expected 0 sort parameters, found " ++ show (length params))

{- | Get the identifier of the @unit@ sort attribute.

Fail if the attribute is missing.
-}
getUnitId ::
    -- | Sort attributes
    Attribute.Sort ->
    Either (Error VerifyError) Id
getUnitId Attribute.Sort{unit = Attribute.Sort.Unit sortUnit} =
    case sortUnit of
        Just SymbolOrAlias{symbolOrAliasConstructor} ->
            return symbolOrAliasConstructor
        Nothing -> Kore.Error.koreFail "Missing 'unit' attribute."

{- | Get the identifier of the @element@ sort attribute.

Fail if the attribute is missing.
-}
getElementId ::
    -- | Sort attributes
    Attribute.Sort ->
    Either (Error VerifyError) Id
getElementId Attribute.Sort{element = Attribute.Sort.Element sortElement} =
    case sortElement of
        Just SymbolOrAlias{symbolOrAliasConstructor} ->
            return symbolOrAliasConstructor
        Nothing -> Kore.Error.koreFail "Missing 'element' attribute."

{- | Get the identifier of the @concat@ sort attribute.

Fail if the attribute is missing.
-}
getConcatId ::
    -- | Sort attributes
    Attribute.Sort ->
    Either (Error VerifyError) Id
getConcatId Attribute.Sort{concat = Attribute.Sort.Concat sortConcat} =
    case sortConcat of
        Just SymbolOrAlias{symbolOrAliasConstructor} ->
            return symbolOrAliasConstructor
        Nothing -> Kore.Error.koreFail "Missing 'concat' attribute."

{- | Check that the symbol's @hook@ attribute matches the expected value.

Fail if the symbol is not defined or the attribute is missing.
-}
assertSymbolHook ::
    IndexedModuleSyntax patternType declAttrs ->
    -- | Symbol identifier
    Id ->
    -- | Expected hook
    Text ->
    Either (Error VerifyError) ()
assertSymbolHook indexedModule symbolId expected = do
    (_, decl) <- IndexedModule.resolveSymbol indexedModule symbolId
    let SentenceSymbol{sentenceSymbolAttributes = attrs} = decl
        SentenceSymbol{sentenceSymbolSymbol = symbol} = decl
    Hook{getHook} <- Verifier.Attributes.parseAttributes attrs
    case getHook of
        Just hook
            | hook == expected -> return ()
            | otherwise ->
                Kore.Error.koreFailWithLocations
                    [symbol]
                    ("Symbol is not hooked to builtin symbol '" <> expected <> "'")
        Nothing ->
            Kore.Error.koreFailWithLocations
                [symbol]
                "Missing 'hook' attribute"

{- | Check that the symbol's result sort matches the expected value.

Fail if the symbol is not defined.
-}
assertSymbolResultSort ::
    IndexedModuleSyntax patternType declAttrs ->
    -- | Symbol identifier
    Id ->
    -- | Expected result sort
    Sort ->
    Either (Error VerifyError) ()
assertSymbolResultSort indexedModule symbolId expectedSort = do
    (_, decl) <- IndexedModule.resolveSymbol indexedModule symbolId
    let SentenceSymbol{sentenceSymbolResultSort = actualSort} = decl
        SentenceSymbol{sentenceSymbolSymbol = symbol} = decl
    unless (actualSort == expectedSort) $
        Kore.Error.koreFailWithLocations
            [symbol]
            ( "Symbol does not return sort '"
                <> unparseToText expectedSort
                <> "'"
            )

{- | Verify the occurrence of a builtin sort.

  Check that the sort is hooked to the named builtin. The sort parameters are
  already checked by the verifier.
-}
verifySort ::
    Text ->
    SortVerifier
verifySort builtinName =
    SortVerifier worker
  where
    worker ::
        (Id -> Either (Error VerifyError) SentenceSort) ->
        Sort ->
        Either (Error VerifyError) ()
    worker findSort (SortActualSort SortActual{sortActualName}) = do
        SentenceSort{sentenceSortAttributes} <- findSort sortActualName
        let expectHook = Hook (Just builtinName)
        declHook <- Verifier.Attributes.parseAttributes sentenceSortAttributes
        Kore.Error.koreFailWhen
            (expectHook /= declHook)
            ( "Sort '"
                ++ getIdForError sortActualName
                ++ "' is not hooked to builtin sort '"
                ++ Text.unpack builtinName
                ++ "'"
            )
    worker _ (SortVariableSort SortVariable{getSortVariable}) =
        Kore.Error.koreFail
            ( "unexpected sort variable '"
                ++ getIdForError getSortVariable
                ++ "'"
            )

-- Verify a sort by only checking if it has domain values.
verifySortHasDomainValues :: SortVerifier
verifySortHasDomainValues = SortVerifier worker
  where
    worker ::
        (Id -> Either (Error VerifyError) SentenceSort) ->
        Sort ->
        Either (Error VerifyError) ()
    worker findSort (SortActualSort SortActual{sortActualName}) = do
        SentenceSort{sentenceSortAttributes} <- findSort sortActualName
        sortAttr <- parseSortAttributes sentenceSortAttributes
        let hasDomainValues =
                Attribute.getHasDomainValues
                    . Attribute.hasDomainValues
                    $ sortAttr
        Kore.Error.koreFailWhen
            (not hasDomainValues)
            ( "Sort '"
                ++ getIdForError sortActualName
                ++ "' does not have domain values."
            )
    worker _ (SortVariableSort SortVariable{getSortVariable}) =
        Kore.Error.koreFail
            ( "unexpected sort variable '"
                ++ getIdForError getSortVariable
                ++ "'"
            )
    parseSortAttributes ::
        Attributes ->
        Either (Error VerifyError) Attribute.Sort
    parseSortAttributes rawSortAttributes =
        Attribute.Parser.liftParser $
            Attribute.Parser.parseAttributes rawSortAttributes

expectDomainValue ::
    Monad m =>
    -- | Context for error message
    Text ->
    -- | Operand pattern
    TermLike variable ->
    MaybeT m Text
expectDomainValue ctx =
    \case
        DV_ _ child ->
            case child of
                StringLiteral_ text ->
                    return text
                _ ->
                    verifierBug $
                        Text.unpack ctx ++ ": Domain value is not a string literal"
        _ -> empty

-- | Wildcard for sort verification on parameterized builtin sorts
acceptAnySort :: SortVerifier
acceptAnySort = SortVerifier $ \_ _ -> return ()

-- | Find the hooked sort for a domain value sort.
lookupHookSort :: Sort -> PatternVerifier (Maybe Text)
lookupHookSort (SortActualSort SortActual{sortActualName}) = do
    SentenceSort{sentenceSortAttributes} <- PatternVerifier.lookupSortDeclaration sortActualName
    Hook mHookSort <- Verifier.Attributes.parseAttributes sentenceSortAttributes
    return mHookSort
lookupHookSort (SortVariableSort SortVariable{getSortVariable}) =
    Kore.Error.koreFail
        ("unexpected sort variable '" ++ getIdForError getSortVariable ++ "'")

{- | Verify a builtin symbol declaration.

  The declared sorts must match the builtin sorts.

  See also: 'verifySymbolArguments'
-}
verifySymbol ::
    -- | Builtin result sort
    SortVerifier ->
    -- | Builtin argument sorts
    [SortVerifier] ->
    SymbolVerifier
verifySymbol verifyResult verifyArguments =
    SymbolVerifier $ \findSort decl -> do
        let SentenceSymbol{sentenceSymbolResultSort = result} = decl
        Kore.Error.withContext
            "In result sort"
            (runSortVerifier verifyResult findSort result)
        runSymbolVerifier (verifySymbolArguments verifyArguments) findSort decl

{- | Verify the arguments of a builtin sort declaration.

  The declared argument sorts must match the builtin argument
  sorts. @verifySymbolArguments@ only checks the symbol's argument sorts; use
  'verifySymbol' if it is also necessary to check the symbol's result sort.

  See also: 'verifySymbol'
-}
verifySymbolArguments ::
    -- | Builtin argument sorts
    [SortVerifier] ->
    SymbolVerifier
verifySymbolArguments verifyArguments =
    SymbolVerifier $ \findSort sentenceSymbol ->
        Kore.Error.withContext "In argument sorts" $ do
            let SentenceSymbol{sentenceSymbolSorts = sorts} = sentenceSymbol
                builtinArity = length verifyArguments
                arity = length sorts
            Kore.Error.koreFailWhen
                (arity /= builtinArity)
                ( "Expected "
                    ++ show builtinArity
                    ++ " arguments, found "
                    ++ show arity
                )
            zipWithM_
                (\verify sort -> verify findSort sort)
                (runSortVerifier <$> verifyArguments)
                sorts

-- | Run a parser on a string.
parseString ::
    Kore.Error.MonadError (Error VerifyError) error =>
    Parser a ->
    Text ->
    error a
parseString parser lit =
    let parsed = Parsec.parse (parser <* Parsec.eof) "<string literal>" lit
     in castParseError parsed
  where
    castParseError =
        either (Kore.Error.koreFail . Parsec.errorBundlePretty) pure
