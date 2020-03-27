{-|
Module      : Kore.Step.SMT.Lemma
Description : Declares all rules marked smt-lemma to the SMT solver.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.SMT.Lemma
    ( declareSMTLemmas
    ) where

import Prelude.Kore

import qualified Control.Comonad.Trans.Cofree as Cofree
import Control.Error
    ( runMaybeT
    )
import qualified Control.Lens as Lens
import qualified Control.Monad.Counter as Counter
import Control.Monad.Except
import qualified Control.Monad.State as State
import qualified Data.Functor.Foldable as Recursive
import Data.Generics.Product.Fields
import qualified Data.Map.Strict as Map
import Data.Reflection
import qualified Data.Text as Text

import qualified Kore.Attribute.Axiom as Attribute
import Kore.Attribute.SmtLemma
import Kore.Attribute.Symbol
import Kore.IndexedModule.IndexedModule
import Kore.IndexedModule.MetadataTools
import Kore.Internal.Predicate
import qualified Kore.Internal.Symbol as Internal.Symbol
import Kore.Internal.TermLike
import qualified Kore.Step.SMT.Declaration.All as SMT.All
    ( declare
    )
import Kore.Step.SMT.Translate
import Kore.Syntax.Sentence
    ( SentenceAxiom (..)
    )
import SMT
    ( MonadSMT (..)
    , SExpr (..)
    )

-- | Given an indexed module, `declareSMTLemmas` translates all
-- rewrite rules marked with the smt-lemma attribute into the
-- smt2 standard, and sends them to the current SMT solver.
-- It assumes that all symbols in all smt-lemma rules either have been
-- declared in the smt prelude or they have an smtlib attribute.
declareSMTLemmas
    :: forall m
    .   ( Given (SmtMetadataTools StepperAttributes)
        , MonadIO m
        , MonadSMT m
        )
    => VerifiedModule StepperAttributes
    -> m ()
declareSMTLemmas m = do
    SMT.All.declare (smtData tools)
    mapM_ declareRule (indexedModuleAxioms m)
  where
    tools :: SmtMetadataTools StepperAttributes
    tools = given

    declareRule
        ::  ( Attribute.Axiom Internal.Symbol.Symbol Variable
            , SentenceAxiom (TermLike Variable)
            )
        -> m (Maybe ())
    declareRule (atts, axiomDeclaration) = runMaybeT $ do
        guard (isSmtLemma $ Attribute.smtLemma atts)
        (lemma, VarContext { terms }) <-
            runTranslator
            $ translatePredicate translateUninterpreted
            $ wrapPredicate $ sentenceAxiomPattern axiomDeclaration
        SMT.assert (addQuantifiers terms lemma)

    addQuantifiers vars lemma | null vars = lemma
    addQuantifiers vars lemma = SMT.List
        [ SMT.Atom "forall"
        , SMT.List
            [ SMT.List [SMT.Atom smtName, smtType]
            | SmtEncoding { smtName, smtType } <- Map.elems vars ]
        , lemma
        ]

translateUninterpreted
    :: forall m variable
     . ( Ord variable
       , Monad m
       )
    => SExpr  -- ^ type name
    -> TranslateItem variable -- ^ uninterpreted pattern
    -> Translator m variable SExpr
translateUninterpreted _ (QuantifiedVariable _) =
    error "Lemma encodings don't support quantified variables (yet)" 
translateUninterpreted t (UninterpretedTerm pat)
  | isVariable pat =
    lookupPattern <|> freeVariable
  | otherwise = empty
  where
    isVariable p =
        case Cofree.tailF $ Recursive.project p of
            VariableF _ -> True
            _ -> False
    lookupPattern = do
        result <- State.gets $ Map.lookup pat . terms
        maybe empty (return . SMT.Atom . smtName) result
    freeVariable = do
        n <- Counter.increment
        let var = "<" <> Text.pack (show n) <> ">"
        State.modify'
            ( Lens.over (field @"terms")
            $ Map.insert pat
                SmtEncoding { smtName = var, smtType = t, boundVars = [] }
            )
        return $ SMT.Atom var
