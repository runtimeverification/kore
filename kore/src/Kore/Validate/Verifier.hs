{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Validate.Verifier (
    Verifier,
    VerifierContext (..),
    VerifierState (..),
    runVerifier,
    --
    VerifiedModule',
    ImplicitModule,
    --
    lookupVerifiedModule,
    lookupParsedModule,
    whileImporting,
) where

import Control.Lens qualified as Lens
import Control.Monad.RWS.Strict (
    MonadReader,
    MonadState,
    RWST,
    runRWST,
 )
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import Data.Binary (Binary)
import Data.Generics.Product
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import GHC.Generics qualified as GHC
import Kore.AST.Error
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Builtin qualified as Builtin
import Kore.Error
import Kore.IndexedModule.IndexedModule as IndexedModule
import Kore.Internal.Symbol qualified as Internal.Symbol (
    Symbol (..),
 )
import Kore.Syntax.Definition
import Kore.Syntax.Variable
import Kore.Validate.Error
import Kore.Verified qualified as Verified
import Prelude.Kore

type ImplicitModule =
    ImplicitIndexedModule
        Verified.Pattern
        Attribute.Symbol
        (Attribute.Axiom Internal.Symbol.Symbol VariableName)

type VerifiedModule' = VerifiedModule Attribute.Symbol

data VerifierContext = VerifierContext
    { implicitModule :: !ImplicitModule
    , modules :: !(Map ModuleName ParsedModule)
    , importing :: ![ModuleName]
    , builtinVerifiers :: !Builtin.Verifiers
    }
    deriving stock (GHC.Generic)

newtype VerifierState = VerifierState
    { verifiedModules :: Map ModuleName VerifiedModule'
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)

newtype Verifier a = Verifier
    { getVerifier ::
        RWST
            VerifierContext
            ()
            VerifierState
            (Either (Error VerifyError))
            a
    }
    deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader VerifierContext Verifier

deriving newtype instance MonadState VerifierState Verifier

deriving newtype instance MonadError (Error VerifyError) Verifier

runVerifier ::
    Verifier a ->
    Map ModuleName VerifiedModule' ->
    ImplicitModule ->
    Map ModuleName (Module ParsedSentence) ->
    Builtin.Verifiers ->
    Either (Error VerifyError) (a, Map ModuleName VerifiedModule')
runVerifier
    moduleVerifier
    alreadyVerifiedModules
    implicitModule
    modules
    builtinVerifiers =
        do
            (a, verifierState', ()) <-
                runRWST
                    (getVerifier moduleVerifier)
                    verifierContext
                    verifierState
            return (a, verifiedModules verifierState')
      where
        verifierState =
            VerifierState{verifiedModules = alreadyVerifiedModules}
        verifierContext =
            VerifierContext
                { implicitModule
                , modules
                , importing = []
                , builtinVerifiers
                }

-- | Find the named 'VerifiedModule' in the cache, if present.
lookupVerifiedModule :: ModuleName -> Verifier (Maybe VerifiedModule')
lookupVerifiedModule name = State.gets (Map.lookup name . verifiedModules)

{- | Find the named 'ParsedModule'.

It is an error if the module is missing.
-}
lookupParsedModule :: ModuleName -> Verifier ParsedModule
lookupParsedModule name =
    Reader.asks (Map.lookup name . modules) >>= maybe notFound return
  where
    notFound =
        koreFail ("Module " ++ getModuleNameForError name ++ " not found.")

{- | Add the 'ModuleName' to the import stack.

It is an error if there is a dependency cycle among modules, i.e. if the
'ModuleName' was already on the stack.
-}
whileImporting :: ModuleName -> Verifier a -> Verifier a
whileImporting name locally = do
    VerifierContext{importing} <- Reader.ask
    let failCycle =
            koreFailWhen
                (elem name importing)
                "Circular module import dependency."
        importing' = name : importing
    foldr withModuleContext failCycle (reverse importing')
    Reader.local (Lens.set (field @"importing") importing') locally
