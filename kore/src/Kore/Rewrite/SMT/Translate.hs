{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.SMT.Translate (
    translatePredicateWith,
    Translator (..),
    TranslateItem (..),
    TranslatorState (..),
    SMTDependentAtom (..),
    translateSMTDependentAtom,
    evalTranslator,
    runTranslator,
    maybeToTranslator,
    -- For testing
    translatePattern,
) where

import Control.Comonad.Trans.Cofree qualified as Cofree
import Control.Error (
    MaybeT,
    hoistMaybe,
 )
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Control.Monad.Counter (
    CounterT,
    MonadCounter,
    evalCounterT,
 )
import Control.Monad.Except
import Control.Monad.Morph as Morph
import Control.Monad.RWS.Strict (
    MonadReader,
    RWST (..),
    ask,
    evalRWST,
    local,
 )
import Control.Monad.State.Strict (
    MonadState,
 )
import Control.Monad.State.Strict qualified as State
import Data.Binary (Binary)
import Data.Default
import Data.Functor.Const
import Data.Functor.Foldable qualified as Recursive
import Data.Generics.Product.Fields
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Kore.Attribute.Hook
import Kore.Attribute.Smtlib
import Kore.Attribute.Sort qualified as Attribute
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Builtin.Bool qualified as Builtin.Bool
import Kore.Builtin.Int qualified as Builtin.Int
import Kore.IndexedModule.MetadataTools
import Kore.Internal.InternalBool
import Kore.Internal.InternalInt
import Kore.Internal.Predicate
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.TermLike
import Kore.Internal.TermLike qualified as TermLike
import Kore.Log.WarnSymbolSMTRepresentation (
    warnSymbolSMTRepresentation,
 )
import Kore.Rewrite.SMT.Resolvers (
    translateSort,
    translateSymbol,
 )
import Kore.Simplify.Simplify (
    MonadSimplify,
 )
import Log (
    MonadLog (..),
 )
import Prelude.Kore
import SMT (
    SExpr (..),
 )
import SMT qualified
import SMT.SimpleSMT qualified as SimpleSMT

data TranslateItem variable
    = QuantifiedVariable !(ElementVariable variable)
    | UninterpretedTerm !(TermLike variable)
    | UninterpretedPredicate !(Predicate variable)

type TranslateTerm variable m =
    SExpr -> TranslateItem variable -> Translator variable m SExpr

-- ----------------------------------------------------------------
-- Predicate translation

{- | Translate a predicate for SMT.

The predicate may inhabit an arbitrary sort. Logical connectives are translated
to their SMT counterparts. Quantifiers, @\\ceil@, @\\floor@, and @\\in@ are
uninterpreted (translated as variables) as is @\\equals@ if its arguments are
not builtins or predicates. All other patterns are not translated and prevent
the predicate from being sent to SMT.
-}
translatePredicateWith ::
    forall p variable m.
    ( p ~ Predicate variable
    , MonadLog m
    , InternalVariable variable
    ) =>
    SmtMetadataTools Attribute.Symbol ->
    SideCondition variable ->
    TranslateTerm variable m ->
    Predicate variable ->
    Translator variable m SExpr
translatePredicateWith tools sideCondition translateTerm predicate =
    translatePredicatePattern predicate
  where
    translatePredicatePattern :: p -> Translator variable m SExpr
    translatePredicatePattern pat =
        case Cofree.tailF (Recursive.project pat) of
            -- Logical connectives: translate as connectives
            Predicate.AndF and' -> translatePredicateAnd and'
            Predicate.BottomF _ -> return (SMT.bool False)
            Predicate.EqualsF eq ->
                -- Equality of predicates and builtins can be translated to
                -- equality in the SMT solver, but other patterns must remain
                -- uninterpreted.
                translatePredicateEquals eq
                    <|> translateUninterpretedPredicate translateTerm SMT.tBool pat
            Predicate.IffF iff -> translatePredicateIff iff
            Predicate.ImpliesF implies -> translatePredicateImplies implies
            Predicate.NotF not' -> translatePredicateNot not'
            Predicate.OrF or' -> translatePredicateOr or'
            Predicate.TopF _ -> return (SMT.bool True)
            -- Uninterpreted: translate as variables
            Predicate.CeilF _ -> translateUninterpretedPredicate translateTerm SMT.tBool pat
            Predicate.ExistsF exists' ->
                translatePredicateExists exists'
                    <|> translateUninterpretedPredicate translateTerm SMT.tBool pat
            Predicate.FloorF _ -> translateUninterpretedPredicate translateTerm SMT.tBool pat
            Predicate.ForallF forall' ->
                translatePredicateForall forall'
                    <|> translateUninterpretedPredicate translateTerm SMT.tBool pat
            Predicate.InF _ -> translateUninterpretedPredicate translateTerm SMT.tBool pat

    translatePredicateAnd And{andFirst, andSecond} =
        SMT.and
            <$> translatePredicatePattern andFirst
            <*> translatePredicatePattern andSecond

    translatePredicateEquals
        Equals
            { equalsFirst
            , equalsSecond
            } =
            SMT.eq
                <$> translatePredicateEqualsChild equalsFirst
                <*> translatePredicateEqualsChild equalsSecond
          where
            translatePredicateEqualsChild child =
                -- Attempt to translate patterns in builtin sorts, or failing that,
                -- as predicates.
                (<|>)
                    ( translatePattern
                        tools
                        sideCondition
                        translateTerm
                        (termLikeSort child)
                        child
                    )
                    ( either
                        (const empty)
                        translatePredicatePattern
                        (makePredicate child)
                    )

    translatePredicateIff Iff{iffFirst, iffSecond} =
        iff
            <$> translatePredicatePattern iffFirst
            <*> translatePredicatePattern iffSecond
      where
        iff a b = SMT.and (SMT.implies a b) (SMT.implies b a)

    translatePredicateImplies Implies{impliesFirst, impliesSecond} =
        SMT.implies
            <$> translatePredicatePattern impliesFirst
            <*> translatePredicatePattern impliesSecond

    translatePredicateNot Not{notChild} =
        SMT.not <$> translatePredicatePattern notChild

    translatePredicateOr Or{orFirst, orSecond} =
        SMT.or
            <$> translatePredicatePattern orFirst
            <*> translatePredicatePattern orSecond

    translatePredicateExists ::
        Exists () variable p -> Translator variable m SExpr
    translatePredicateExists Exists{existsVariable, existsChild} =
        translateQuantifier SMT.existsQ existsVariable existsChild

    translatePredicateForall ::
        Forall () variable p -> Translator variable m SExpr
    translatePredicateForall Forall{forallVariable, forallChild} =
        translateQuantifier SMT.forallQ forallVariable forallChild

    translateQuantifier ::
        ([SExpr] -> SExpr -> SExpr) ->
        ElementVariable variable ->
        p ->
        Translator variable m SExpr
    translateQuantifier quantifier var predTerm = do
        smtSort <- translateVariableSort
        oldVar <- State.gets (Map.lookup var . quantifiedVars)
        smtVar <- translateTerm smtSort (QuantifiedVariable var)
        smtPred <- translatePredicatePattern predTerm
        field @"quantifiedVars"
            Lens.%= maybe (Map.delete var) (Map.insert var) oldVar
        return $ quantifier [SMT.List [smtVar, smtSort]] smtPred
      where
        Variable{variableSort} = var
        translateVariableSort :: Translator variable m SExpr
        translateVariableSort =
            case getHook of
                Just builtinSort
                    | builtinSort == Builtin.Bool.sort -> pure SMT.tBool
                    | builtinSort == Builtin.Int.sort -> pure SMT.tInt
                _ -> translateSort tools variableSort & maybeToTranslator
        Attribute.Sort{hook = Hook{getHook}} =
            sortAttributes tools variableSort

{- | Attempt to translate an arbitrary ML pattern for the solver.
 It should only be used in the 'Predicate' translator or in
 the tests.
-}
translatePattern ::
    forall variable monad.
    MonadLog monad =>
    InternalVariable variable =>
    SmtMetadataTools Attribute.Symbol ->
    SideCondition variable ->
    TranslateTerm variable monad ->
    Sort ->
    TermLike variable ->
    Translator variable monad SExpr
translatePattern tools sideCondition translateTerm sort pat =
    case getHook of
        Just builtinSort
            | builtinSort == Builtin.Bool.sort -> translateBool pat
            | builtinSort == Builtin.Int.sort -> translateInt pat
        _ -> case Cofree.tailF $ Recursive.project pat of
            VariableF _ -> do
                smtSort <- maybeToTranslator $ translateSort tools sort
                translateUninterpreted translateTerm smtSort pat
            ApplySymbolF app ->
                translateApplication (translateSort tools sort) pat app
            _ -> empty
  where
    Attribute.Sort{hook = Hook{getHook}} =
        sortAttributes tools sort

    translateInt :: TermLike variable -> Translator variable monad SExpr
    translateInt pat'
        | SideCondition.isDefined sideCondition pat' =
            withDefinednessAssumption $
                translateIntWorker pat'
        | otherwise =
            translateIntWorker pat'

    translateIntWorker :: TermLike variable -> Translator variable monad SExpr
    translateIntWorker pat' =
        case Cofree.tailF (Recursive.project pat') of
            VariableF _ -> translateUninterpreted translateTerm SMT.tInt pat'
            InternalIntF (Const InternalInt{internalIntValue}) ->
                return $ SMT.int internalIntValue
            ApplySymbolF app ->
                translateApplication (Just SMT.tInt) pat' app
            _ -> empty
    translateBool :: TermLike variable -> Translator variable monad SExpr
    translateBool pat'
        | SideCondition.isDefined sideCondition pat' =
            withDefinednessAssumption $
                translateBoolWorker pat'
        | otherwise =
            translateBoolWorker pat'

    translateBoolWorker :: TermLike variable -> Translator variable monad SExpr
    translateBoolWorker pat' =
        case Cofree.tailF (Recursive.project pat') of
            TermLike.VariableF _ -> translateUninterpreted translateTerm SMT.tBool pat'
            TermLike.InternalBoolF (Const InternalBool{internalBoolValue}) ->
                return $ SMT.bool internalBoolValue
            TermLike.NotF Not{notChild} ->
                -- \not is equivalent to BOOL.not for total patterns.
                -- The following is safe because non-total patterns
                -- will fail to translate.
                SMT.not <$> translateBool notChild
            TermLike.ApplySymbolF app ->
                translateApplication (Just SMT.tBool) pat' app
            _ -> empty

    translateApplication ::
        Maybe SExpr ->
        TermLike variable ->
        Application Symbol (TermLike variable) ->
        Translator variable monad SExpr
    translateApplication
        maybeSort
        original
        Application
            { applicationSymbolOrAlias
            , applicationChildren
            } =
            -- TODO: This would send interpreted symbols to the solver
            -- even if they may not be defined. We should only send symbols
            -- we know to be defined.
            translateInterpretedApplication
                <|> translateUninterpreted'
          where
            guardLocalTotalPattern
                | isTotalPattern original = return ()
                | otherwise = do
                    TranslatorEnv{assumeDefined} <- ask
                    Monad.guard (assumeDefined && isFunctionPattern original)
            translateInterpretedApplication = do
                let translated = translateSymbol tools applicationSymbolOrAlias
                sexpr <- maybe warnAndDiscard return translated
                children <-
                    zipWithM
                        (translatePattern tools sideCondition translateTerm)
                        applicationChildrenSorts
                        applicationChildren
                return $ shortenSExpr (applySExpr sexpr children)
            applicationChildrenSorts = termLikeSort <$> applicationChildren
            warnAndDiscard =
                warnSymbolSMTRepresentation applicationSymbolOrAlias
                    >> empty
            translateUninterpreted' = do
                guardLocalTotalPattern
                sort' <- maybeToTranslator maybeSort
                translateUninterpreted translateTerm sort' original

translateUninterpreted ::
    TranslateTerm variable m ->
    SExpr ->
    TermLike variable ->
    Translator variable m SExpr
translateUninterpreted translateTerm sExpr pat =
    translateTerm sExpr (UninterpretedTerm pat)

translateUninterpretedPredicate ::
    TranslateTerm variable m ->
    SExpr ->
    Predicate variable ->
    Translator variable m SExpr
translateUninterpretedPredicate translateTerm sExpr predicate =
    translateTerm sExpr (UninterpretedPredicate predicate)

{- | Represents the SMT encoding of an untranslatable pattern containing
occurrences of existential variables.  Since the same pattern might appear
under different instances of the same existential quantifiers, it is made
dependent on the name of the variables, which must be instantiated with
the current encodings corresponding to those variables when transformed to a
proper 'SExpr'. See 'translateSMTDependentAtom'.
-}
data SMTDependentAtom variable = SMTDependentAtom
    { smtName :: !Text
    , smtType :: !SExpr
    , boundVars :: ![ElementVariable variable]
    }
    deriving stock (Eq, GHC.Generic, Show)
    deriving anyclass (Binary)

{- | Instantiates an 'SMTDependentAtom' with the current encodings for the
variables it depends on.

May fail (return 'empty') if any of the variables it depends on are not
currently existentially quantified.
-}
translateSMTDependentAtom ::
    InternalVariable variable =>
    SMT.MonadSMT m =>
    Map.Map (ElementVariable variable) (SMTDependentAtom variable) ->
    SMTDependentAtom variable ->
    Translator variable m SExpr
translateSMTDependentAtom
    quantifiedVars
    SMTDependentAtom{smtName = funName, boundVars} =
        maybe empty (return . SimpleSMT.fun funName) boundEncodings
      where
        boundEncodings =
            for boundVars $
                \name -> SMT.Atom . smtName <$> Map.lookup name quantifiedVars

-- ----------------------------------------------------------------
-- Translator
data TranslatorState variable = TranslatorState
    { terms :: !(Map (TermLike variable) (SMTDependentAtom variable))
    , predicates :: !(Map (Predicate variable) (SMTDependentAtom variable))
    , freeVars :: !(Map (ElementVariable variable) SExpr)
    , quantifiedVars ::
        !(Map (ElementVariable variable) (SMTDependentAtom variable))
    }
    deriving stock (Eq, GHC.Generic, Show)
    deriving anyclass (Binary)

instance Default (TranslatorState variable) where
    def = TranslatorState def def def def

{- | Translator local environment, used to check if a subterm is
 assumed to be defined. If it is, we can translate it for the solver.
-}
newtype TranslatorEnv = TranslatorEnv {assumeDefined :: Bool}

instance Default TranslatorEnv where
    def = TranslatorEnv False

newtype Translator variable m a = Translator
    { getTranslator ::
        MaybeT
            ( RWST
                TranslatorEnv
                ()
                (TranslatorState variable)
                (CounterT m)
            )
            a
    }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (Alternative)
    deriving newtype (MonadCounter, MonadLog)
    deriving newtype (MonadIO)
    deriving newtype (MonadReader TranslatorEnv)
    deriving newtype (MonadState (TranslatorState variable))

instance MonadTrans (Translator variable) where
    lift = Translator . lift . lift . lift

instance MFunctor (Translator variable) where
    hoist f (Translator translator) =
        hoist (hoist (hoist f)) translator
            & Translator

instance SMT.MonadSMT m => SMT.MonadSMT (Translator variable m)

instance MonadSimplify m => MonadSimplify (Translator variable m)

evalTranslator :: Monad m => Translator p m a -> MaybeT m a
evalTranslator (Translator translator) =
    Morph.hoist (evalCounterT . evalRST def def) translator
  where
    evalRST env state rwst = do
        (result, _) <- evalRWST rwst env state
        return result

runTranslator :: Monad m => Translator p m a -> MaybeT m (a, TranslatorState p)
runTranslator = evalTranslator . includeState
  where
    includeState comp = do
        comp' <- comp
        state <- State.get
        pure (comp', state)

maybeToTranslator :: Monad m => Maybe a -> Translator p m a
maybeToTranslator = Translator . hoistMaybe

withDefinednessAssumption :: Monad m => Translator p m a -> Translator p m a
withDefinednessAssumption =
    local (const $ TranslatorEnv True)
