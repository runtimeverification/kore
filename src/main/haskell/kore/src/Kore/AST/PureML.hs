{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Kore.AST.PureML
Description : Specifies the "pure" version of patterns, sentences, modules, and
              definition, which can be specialized to 'Object'-only and
              'Meta'-only objects.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Kore.AST.PureML where

import Data.Functor.Foldable

import Kore.AST.Common
import Kore.AST.Sentence

{-|'PureMLPattern' corresponds to "fixed point" representations
of the 'Pattern' class where the level is fixed to a given @level@.

@var@ is the type of variables.
-}
type PureMLPattern level domain var = Fix (Pattern level domain var)

asPurePattern
    :: Pattern level domain var (PureMLPattern level domain var) -> PureMLPattern level domain var
asPurePattern = embed

fromPurePattern
    :: PureMLPattern level domain var -> Pattern level domain var (PureMLPattern level domain var)
fromPurePattern = project

-- |'PureSentenceAxiom' is the pure (fixed-@level@) version of 'SentenceAxiom'
type PureSentenceAxiom level domain =
    SentenceAxiom (SortVariable level) (Pattern level) domain Variable
-- |'PureSentenceAlias' is the pure (fixed-@level@) version of 'SentenceAlias'
type PureSentenceAlias level domain =
    SentenceAlias level (Pattern level) domain Variable
-- |'PureSentenceSymbol' is the pure (fixed-@level@) version of 'SentenceSymbol'
type PureSentenceSymbol level domain =
    SentenceSymbol level (Pattern level) domain Variable
-- |'PureSentenceImport' is the pure (fixed-@level@) version of 'SentenceImport'
type PureSentenceImport level domain =
    SentenceImport (Pattern level) domain Variable

-- |'PureSentence' is the pure (fixed-@level@) version of 'Sentence'
type PureSentence level domain =
    Sentence level (SortVariable level) (Pattern level) domain Variable

instance AsSentence (PureSentence level domain) (PureSentenceAlias level domain) where
    asSentence = SentenceAliasSentence

instance AsSentence (PureSentence level domain) (PureSentenceSymbol level domain) where
    asSentence = SentenceSymbolSentence

-- |'PureModule' is the pure (fixed-@level@) version of 'Module'
type PureModule level domain =
    Module (Sentence level) (SortVariable level) (Pattern level) domain Variable

-- |'PureDefinition' is the pure (fixed-@level@) version of 'Definition'
type PureDefinition level domain =
    Definition
        (Sentence level) (SortVariable level) (Pattern level) domain Variable

-- |Given an 'Id', 'groundHead' produces the head of an 'Application'
-- corresponding to that argument.
groundHead :: String -> AstLocation -> SymbolOrAlias level
groundHead ctor location = SymbolOrAlias
    { symbolOrAliasConstructor = Id
        { getId = ctor
        , idLocation = location
        }
    , symbolOrAliasParams = []
    }

-- |Given an 'Id', 'groundSymbol' produces the unparameterized 'Symbol'
-- corresponding to that argument.
groundSymbol :: Id level -> Symbol level
groundSymbol ctor = Symbol
    { symbolConstructor = ctor
    , symbolParams = []
    }

-- |Given a head and a list of children, produces an 'ApplicationPattern'
--  applying the given head to the children
apply :: SymbolOrAlias level -> [child] -> Pattern level domain variable child
apply patternHead patterns = ApplicationPattern Application
    { applicationSymbolOrAlias = patternHead
    , applicationChildren = patterns
    }

-- |Applies the given head to the empty list of children to obtain a
-- constant 'ApplicationPattern'
constant
    :: SymbolOrAlias level -> Pattern level domain variable child
constant patternHead = apply patternHead []

-- |'CommonPurePattern' is the instantiation of 'PureMLPattern' with common
-- 'Variable's.
type CommonPurePattern level domain = PureMLPattern level domain Variable
type UnFixedPureMLPattern level domain variable =
    Pattern level domain variable (PureMLPattern level domain variable)
type UnfixedCommonPurePattern level domain = UnFixedPureMLPattern level domain Variable

type PurePatternStub level domain variable =
    PatternStub level domain variable (PureMLPattern level domain variable)

type CommonPurePatternStub level domain =
    PurePatternStub level domain Variable

{-| 'mapPatternVariables' replaces all variables in a 'PureMLPattern'
using the provided mapping.
-}
mapPatternVariables
    :: (variableFrom level -> variableTo level)
    -> PureMLPattern level domain variableFrom
    -> PureMLPattern level domain variableTo
mapPatternVariables mapper = cata (Fix . mapPatternVariable mapper)

{-| 'mapPatternVariables' replaces the variables occurring directly
(i.e. without recursion) in a 'Pattern', using the provided mapping.
-}
mapPatternVariable
    :: (variableFrom level -> variableTo level)
    -> Pattern level domain variableFrom child
    -> Pattern level domain variableTo child
mapPatternVariable _ (AndPattern (And a b c)) =
    AndPattern (And a b c)
mapPatternVariable _ (ApplicationPattern (Application a b)) =
    ApplicationPattern (Application a b)
mapPatternVariable _ (BottomPattern (Bottom a)) =
    BottomPattern (Bottom a)
mapPatternVariable _ (CeilPattern (Ceil a b c)) =
    CeilPattern (Ceil a b c)
mapPatternVariable _ (DomainValuePattern (DomainValue a b)) =
    DomainValuePattern (DomainValue a b)
mapPatternVariable _ (EqualsPattern (Equals a b c d)) =
    EqualsPattern (Equals a b c d)
mapPatternVariable wrapper (ExistsPattern exists) =
    ExistsPattern exists
        { existsVariable = wrapper (existsVariable exists)
        }
mapPatternVariable _ (FloorPattern (Floor a b c)) =
    FloorPattern (Floor a b c)
mapPatternVariable wrapper (ForallPattern forall) =
    ForallPattern forall
        { forallVariable = wrapper (forallVariable forall)}
mapPatternVariable _ (IffPattern (Iff a b c)) =
    IffPattern (Iff a b c)
mapPatternVariable _ (ImpliesPattern (Implies a b c)) =
    ImpliesPattern (Implies a b c)
mapPatternVariable _ (InPattern (In a b c d)) =
    InPattern (In a b c d)
mapPatternVariable _ (NextPattern (Next a b)) =
    NextPattern (Next a b)
mapPatternVariable _ (NotPattern (Not a b)) =
    NotPattern (Not a b)
mapPatternVariable _ (OrPattern (Or a b c)) =
    OrPattern (Or a b c)
mapPatternVariable _ (RewritesPattern (Rewrites a b c)) =
    RewritesPattern (Rewrites a b c)
mapPatternVariable _ (StringLiteralPattern (StringLiteral a)) =
    StringLiteralPattern (StringLiteral a)
mapPatternVariable _ (CharLiteralPattern (CharLiteral a)) =
    CharLiteralPattern (CharLiteral a)
mapPatternVariable _ (TopPattern (Top a)) =
    TopPattern (Top a)
mapPatternVariable wrapper (VariablePattern variable) =
    VariablePattern (wrapper variable)
