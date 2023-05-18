{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Validate.PatternVerifier.PatternVerifier (
    PatternVerifier (..),
    runPatternVerifier,
    Context (..),
    verifiedModuleContext,

    -- * Declared variables
    DeclaredVariables (..),
    emptyDeclaredVariables,
    withDeclaredVariables,
    lookupDeclaredVariable,
    addDeclaredVariable,
    newDeclaredVariable,
    uniqueDeclaredVariables,

    -- * Hooks
    PatternVerifierHook (..),

    -- * Utilities
    lookupSortDeclaration,
    lookupAlias,
    lookupSymbol,
    assertExpectedSort,
    assertSameSort,
    applicationSortsFromSymbolOrAliasSentence,
) where

import Control.Monad (
    (>=>),
 )
import Control.Monad.Reader (
    MonadReader,
    ReaderT,
    runReaderT,
 )
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Maybe
import Data.Binary (Binary)
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import GHC.Generics qualified as GHC
import Kore.AST.Error
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Error
import Kore.IndexedModule.IndexedModule
import Kore.IndexedModule.Resolvers
import Kore.Internal.Alias qualified as Internal
import Kore.Internal.ApplicationSorts
import Kore.Internal.Symbol qualified as Internal
import Kore.Syntax as Syntax
import Kore.Syntax.Definition
import Kore.Unparser
import Kore.Validate.Error
import Kore.Validate.SortVerifier
import Kore.Verified qualified as Verified
import Prelude.Kore
import Pretty (
    (<+>),
 )
import Pretty qualified

newtype DeclaredVariables = DeclaredVariables
    { getDeclaredVariables ::
        Map.Map
            (SomeVariableName VariableName)
            (SomeVariable VariableName)
    }
    deriving newtype (Monoid, Semigroup)
    deriving newtype (Binary)

emptyDeclaredVariables :: DeclaredVariables
emptyDeclaredVariables = mempty

{- | 'PatternVerifierHook' is a hook run after verifying a 'Verified.Pattern'.

These are usually used to construct and verify internal representations after
parsing.

The 'Semigroup' instance is used to compose 'PatternVerifierHook's to be applied
in sequence.
-}
newtype PatternVerifierHook = PatternVerifierHook
    { runPatternVerifierHook ::
        Verified.Pattern ->
        PatternVerifier Verified.Pattern
    }

instance Semigroup PatternVerifierHook where
    (<>) verifier1 verifier2 =
        PatternVerifierHook
            (on (>=>) runPatternVerifierHook verifier1 verifier2)
    {-# INLINE (<>) #-}

instance Monoid PatternVerifierHook where
    mempty = PatternVerifierHook return
    {-# INLINE mempty #-}

data Context = Context
    { declaredVariables :: !DeclaredVariables
    , declaredSortVariables :: !(Set SortVariable)
    -- ^ The sort variables in scope.
    , indexedModule ::
        !(IndexedModuleSyntax Verified.Pattern Attribute.Symbol)
    -- ^ The indexed Kore module containing all definitions in scope.
    , patternVerifierHook :: !PatternVerifierHook
    , isRpcRequest :: Bool
    }
    deriving stock (GHC.Generic)

verifiedModuleContext :: VerifiedModuleSyntax Attribute.Symbol -> Context
verifiedModuleContext indexedModule =
    Context
        { declaredVariables = mempty
        , declaredSortVariables = mempty
        , indexedModule
        , patternVerifierHook = mempty
        , isRpcRequest = False
        }

newtype PatternVerifier a = PatternVerifier
    {getPatternVerifier :: ReaderT Context (Either (Error VerifyError)) a}
    deriving newtype (Applicative, Functor, Monad)
    deriving newtype (MonadReader Context)

deriving newtype instance
    e ~ VerifyError => MonadError (Error e) PatternVerifier

runPatternVerifier ::
    Context ->
    PatternVerifier a ->
    Either (Error VerifyError) a
runPatternVerifier ctx PatternVerifier{getPatternVerifier} =
    runReaderT getPatternVerifier ctx

lookupSortDeclaration :: Id -> PatternVerifier Verified.SentenceSort
lookupSortDeclaration sortId = do
    Context{indexedModule} <- Reader.ask
    (_, sortDecl) <- resolveSort indexedModule sortId
    return sortDecl

lookupAlias :: SymbolOrAlias -> MaybeT PatternVerifier Verified.Alias
lookupAlias symbolOrAlias = do
    Context{indexedModule} <- Reader.ask
    let resolveAlias' = resolveAlias indexedModule aliasConstructor
    (_, decl) <- resolveAlias' `catchError` const empty
    aliasSorts <-
        Trans.lift $
            applicationSortsFromSymbolOrAliasSentence symbolOrAlias decl
    let aliasLeft = leftDefinition decl
        aliasRight = sentenceAliasRightPattern decl
    return
        Internal.Alias
            { aliasConstructor
            , aliasParams
            , aliasSorts
            , aliasLeft
            , aliasRight
            }
  where
    aliasConstructor = symbolOrAliasConstructor symbolOrAlias
    aliasParams = symbolOrAliasParams symbolOrAlias
    leftDefinition def =
        applicationChildren
            . sentenceAliasLeftPattern
            $ def

lookupSymbol :: SymbolOrAlias -> MaybeT PatternVerifier Internal.Symbol
lookupSymbol symbolOrAlias = do
    Context{indexedModule} <- Reader.ask
    let resolveSymbol' = resolveSymbol indexedModule symbolConstructor
    (symbolAttributes, decl) <- resolveSymbol' `catchError` const empty
    symbolSorts <-
        Trans.lift $
            applicationSortsFromSymbolOrAliasSentence symbolOrAlias decl
    let symbol =
            Internal.Symbol
                { symbolConstructor
                , symbolParams
                , symbolAttributes
                , symbolSorts
                }
    return symbol
  where
    symbolConstructor = symbolOrAliasConstructor symbolOrAlias
    symbolParams = symbolOrAliasParams symbolOrAlias

lookupDeclaredVariable ::
    SomeVariableName VariableName ->
    PatternVerifier (SomeVariable VariableName)
lookupDeclaredVariable varId = do
    variables <- Reader.asks (getDeclaredVariables . declaredVariables)
    maybe errorUnquantified return $ Map.lookup varId variables
  where
    errorUnquantified :: PatternVerifier (SomeVariable VariableName)
    errorUnquantified =
        koreFailWithLocations [varId] $
            Pretty.renderText . Pretty.layoutOneLine $
                Pretty.hsep
                    [ "Unquantified variable:"
                    , unparse varId
                    ]

addDeclaredVariable ::
    SomeVariable VariableName ->
    DeclaredVariables ->
    DeclaredVariables
addDeclaredVariable variable (getDeclaredVariables -> variables) =
    DeclaredVariables $
        Map.insert
            (variableName variable)
            variable
            variables

{- | Add a new variable to the set of 'DeclaredVariables'.

The new variable must not already be declared.
-}
newDeclaredVariable ::
    DeclaredVariables ->
    SomeVariable VariableName ->
    PatternVerifier DeclaredVariables
newDeclaredVariable declared variable = do
    let declaredVariables = getDeclaredVariables declared
    case Map.lookup name declaredVariables of
        Just variable' -> alreadyDeclared variable'
        Nothing -> return (addDeclaredVariable variable declared)
  where
    name = variableName variable
    alreadyDeclared ::
        SomeVariable VariableName -> PatternVerifier DeclaredVariables
    alreadyDeclared variable' =
        koreFailWithLocations [variable', variable] $
            Pretty.renderText . Pretty.layoutOneLine $
                Pretty.hsep
                    [ "Variable"
                    , unparse name
                    , "was already declared."
                    ]

{- | Collect 'DeclaredVariables'.

Each variable in the 'Foldable' collection must be unique.

See also: 'newDeclaredVariable'
-}
uniqueDeclaredVariables ::
    Foldable f =>
    f (SomeVariable VariableName) ->
    PatternVerifier DeclaredVariables
uniqueDeclaredVariables = foldlM newDeclaredVariable emptyDeclaredVariables

{- | Run a 'PatternVerifier' in a particular variable context.

See also: 'verifyStandalonePattern'
-}
withDeclaredVariables ::
    DeclaredVariables ->
    PatternVerifier a ->
    PatternVerifier a
withDeclaredVariables declaredVariables' =
    Reader.local (\ctx -> ctx{declaredVariables = declaredVariables'})

applicationSortsFromSymbolOrAliasSentence ::
    SentenceSymbolOrAlias sentence =>
    SymbolOrAlias ->
    sentence ->
    PatternVerifier ApplicationSorts
applicationSortsFromSymbolOrAliasSentence symbolOrAlias sentence = do
    Context{declaredSortVariables} <- Reader.ask
    mapM_
        (verifySort lookupSortDeclaration declaredSortVariables)
        (symbolOrAliasParams symbolOrAlias)
    symbolOrAliasSorts (symbolOrAliasParams symbolOrAlias) sentence

assertSameSort ::
    Sort ->
    Sort ->
    PatternVerifier ()
assertSameSort expectedSort actualSort =
    koreFailWithLocationsWhen
        (expectedSort /= actualSort)
        [expectedSort, actualSort]
        $ Pretty.renderText . Pretty.layoutCompact
        $ "Expecting sort"
            <+> unparse expectedSort
            <+> "but got"
            <+> unparse actualSort
                <> Pretty.dot

assertExpectedSort ::
    Maybe Sort ->
    Sort ->
    PatternVerifier ()
assertExpectedSort Nothing _ = return ()
assertExpectedSort (Just expected) actual =
    assertSameSort expected actual
