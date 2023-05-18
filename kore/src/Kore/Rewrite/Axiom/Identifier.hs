{- |
Module      : Kore.Rewrite.Axiom.Identifier
Description : Data structures and manipulation helpers used for axiom
              evaluation identifiers.
              The expectation is that an axiom can be applied to a term
              only if the identifier of its left-hand-side is the same
              as the term's identifier.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
This module is intended to be imported qualified:
@
import Kore.Rewrite.Axiom.Identifier ( AxiomIdentifier )
import Kore.Rewrite.Axiom.Identifier as AxiomIdentifier
@
-}
module Kore.Rewrite.Axiom.Identifier (
    AxiomIdentifier (..),
    matchAxiomIdentifier,
) where

import Data.Binary (Binary)
import Data.Functor.Const (
    Const (..),
 )
import Data.Functor.Foldable qualified as Recursive
import Data.HashMap.Strict qualified as HashMap
import Data.Sequence qualified as Seq
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Builtin.Endianness.Endianness qualified as Endianness
import Kore.Builtin.Signedness.Signedness qualified as Signedness
import Kore.Debug
import Kore.Internal.Alias qualified as Alias
import Kore.Internal.Inj qualified as Inj
import Kore.Internal.InternalList
import Kore.Internal.InternalSet
import Kore.Internal.Symbol qualified as Symbol
import Kore.Internal.TermLike (
    TermLike,
    TermLikeF (..),
 )
import Kore.Syntax.Application qualified as Syntax
import Kore.Syntax.Ceil qualified as Syntax
import Kore.Syntax.Equals qualified as Syntax
import Kore.Syntax.Exists qualified as Syntax
import Kore.Syntax.Id (
    Id (..),
 )
import Kore.Syntax.Not qualified as Syntax (notChild)
import Kore.Unparser (
    unparse,
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

{- | Identifer for the left-hand-side of axioms and for the terms with which
these can be identified.

The expectation is that an axiom can be applied to a term only if the
identifier of its left-hand-side is the same as the term's identifier.
-}
data AxiomIdentifier
    = -- | An application pattern with the given symbol identifier.
      Application !Id
    | -- | Any domain value pattern.
      DV
    | -- | A @\\ceil@ pattern with the given child.
      Ceil !AxiomIdentifier
    | -- | An @\\equals@ pattern with the given children.
      Equals !AxiomIdentifier !AxiomIdentifier
    | -- | An @\\exists@ pattern with the given child.
      Exists !AxiomIdentifier
    | -- | A @\\not@ pattern with the given child.
      Not !AxiomIdentifier
    | -- | Any variable pattern.
      Variable
    | -- | Anything else.
      Other
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Pretty AxiomIdentifier where
    pretty (Application name) = unparse name
    pretty DV = "\\dv{_}(_)"
    pretty (Ceil axiomIdentifier) =
        "\\ceil" <> Pretty.parens (pretty axiomIdentifier)
    pretty (Equals first second) =
        "\\equals"
            <> Pretty.parens
                (pretty first Pretty.<+> "," Pretty.<+> pretty second)
    pretty (Exists axiomIdentifier) =
        "\\exists" <> Pretty.parens (pretty axiomIdentifier)
    pretty (Not axiomIdentifier) =
        "\\not" <> Pretty.parens (pretty axiomIdentifier)
    pretty Variable = "_"
    pretty Other = "?"

{- | Match 'TermLike' pattern to determine its 'AxiomIdentifier'.

Returns 'Nothing' if the 'TermLike' does not conform to one of the structures we
recognize.
-}
matchAxiomIdentifier ::
    TermLike variable ->
    AxiomIdentifier
matchAxiomIdentifier = Recursive.fold matchWorker
  where
    matchWorker (_ :< termLikeF) =
        case termLikeF of
            ApplyAliasF application -> mkAliasId application
            ApplySymbolF application -> mkAppId application
            CeilF ceil -> Ceil $ Syntax.ceilChild ceil
            EqualsF equals ->
                Equals
                    (Syntax.equalsFirst equals)
                    (Syntax.equalsSecond equals)
            ExistsF exists -> Exists $ Syntax.existsChild exists
            NotF not' ->
                Not $ Syntax.notChild not'
            VariableF _ ->
                Variable
            EndiannessF endiannessF ->
                mkAppId $
                    Endianness.toApplication $
                        getConst endiannessF
            SignednessF signednessF ->
                mkAppId $
                    Signedness.toApplication $
                        getConst signednessF
            InjF inj -> mkAppId $ Inj.toApplication inj
            InternalListF internalList -> listToId internalList
            InternalSetF internalSet -> acToId internalSet
            InternalMapF internalMap -> acToId internalMap
            DomainValueF _ -> DV
            InternalBoolF _ -> DV
            InternalBytesF _ -> DV
            InternalIntF _ -> DV
            InternalStringF _ -> DV
            -- Anything we do not want to handle specially returns this.
            _ -> Other

    listToId
        InternalList
            { internalListChild = list
            , internalListUnit = unitSymbol
            , internalListElement = elementSymbol
            , internalListConcat = concatSymbol
            }
            | Seq.null list = Application $ symbolToId unitSymbol
            | Seq.length list == 1 = Application $ symbolToId elementSymbol
            | otherwise = Application $ symbolToId concatSymbol

    acToId ::
        AcWrapper wrapper =>
        InternalAc k wrapper AxiomIdentifier ->
        AxiomIdentifier
    acToId
        InternalAc
            { builtinAcChild
            , builtinAcUnit = unitSymbol
            , builtinAcElement = elementSymbol
            , builtinAcConcat = concatSymbol
            } =
            acToId'
                unitSymbol
                elementSymbol
                concatSymbol
                elementsWithVariables
                (HashMap.toList concreteElements)
                opaque
          where
            NormalizedAc
                { elementsWithVariables
                , concreteElements
                , opaque
                } = unwrapAc builtinAcChild

    acToId' unitSymbol _ _ [] [] [] = Application $ symbolToId unitSymbol
    acToId' _ elementSymbol _ [_] [] [] = Application $ symbolToId elementSymbol
    acToId' _ elementSymbol _ [] [_] [] = Application $ symbolToId elementSymbol
    acToId' _ _ _ [] [] [opaque] = opaque
    acToId' _ _ concatSymbol _ _ _ = Application $ symbolToId concatSymbol

    mkAppId = Application . symbolToId . Syntax.applicationSymbolOrAlias
    mkAliasId = Application . aliasToId . Syntax.applicationSymbolOrAlias
    symbolToId = Syntax.symbolOrAliasConstructor . Symbol.toSymbolOrAlias
    aliasToId = Syntax.symbolOrAliasConstructor . Alias.toSymbolOrAlias
