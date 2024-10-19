{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

Attributes stored together with different entities in a
@KoreDefinition@.
-}
module Booster.Definition.Attributes.Base (
    DefinitionAttributes (..),
    defaultDefAttributes,
    ModuleAttributes (..),
    AxiomAttributes (..),
    Concreteness (..),
    Constrained (..),
    ComputedAxiomAttributes (..),
    SymbolType (..),
    FunctionType (..),
    SymbolAttributes (..),
    SMTType (..),
    SortAttributes (..),
    KCollectionTag (..),
    KCollectionSymbolNames (..),
    KCollectionMetadata (..),
    KMapDefinition (..),
    KListDefinition (..),
    KSetDefinition,
    Label,
    UniqueId (..),
    Location (..),
    Position (..),
    FileSource (..),
    Priority (..),
    SyntacticClauses (..),
    pattern IsIdem,
    pattern IsNotIdem,
    pattern IsAssoc,
    pattern IsNotAssoc,
    pattern IsMacroOrAlias,
    pattern IsNotMacroOrAlias,
    pattern CanBeEvaluated,
    pattern CannotBeEvaluated,
    NotPreservesDefinednessReason (..),
) where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.String
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Prettyprinter as Pretty

import Booster.SMT.Base (SExpr)
import Booster.Util (Flag (..))
import Booster.Util qualified as Util

newtype DefinitionAttributes = DefinitionAttributes
    { indexCells :: [ByteString]
    -- ^ which cells to index for rewriting. Empty => default
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

defaultDefAttributes :: DefinitionAttributes
defaultDefAttributes = DefinitionAttributes []

data ModuleAttributes = ModuleAttributes
    {
    }
    -- none needed

    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

{- | Things needed for booster rewrite engine:
  * axiom location (for debug logging and error messages)
  * priority (to order and group axioms by descending priority)
  * label (to implement cut-point support)
-}
data AxiomAttributes = AxiomAttributes
    { location :: Maybe Location
    , priority :: Priority -- priorities are <= 200
    , ruleLabel :: Maybe Label
    , uniqueId :: UniqueId
    , simplification :: Flag "isSimplification"
    , preserving :: Flag "preservingDefinedness" -- this will override the computed attribute
    , concreteness :: Concreteness
    , smtLemma :: Flag "isSMTLemma"
    , syntacticClauses :: SyntacticClauses -- indices of conjuncts in rule.requires to be checked syntactically
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

data ComputedAxiomAttributes = ComputedAxiomAttributes
    { containsAcSymbols :: Bool
    , notPreservesDefinednessReasons :: [NotPreservesDefinednessReason]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

newtype NotPreservesDefinednessReason = UndefinedSymbol ByteString
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

instance Pretty NotPreservesDefinednessReason where
    pretty = \case
        UndefinedSymbol name -> "non-total symbol " <> (pretty $ Text.decodeUtf8 $ Util.decodeLabel' name)

instance ToJSON NotPreservesDefinednessReason where
    toJSON (UndefinedSymbol n) = JSON.String $ Text.decodeUtf8 n

type Label = Text

newtype UniqueId = UniqueId {getUniqueId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

newtype Priority = Priority Word8
    deriving stock (Eq, Ord, Read, Show, Bounded)
    deriving newtype (Num, NFData)

newtype FileSource = FileSource FilePath
    deriving stock (Eq, Ord, Show)
    deriving newtype (IsString, NFData, Pretty)

data Location = Location
    { file :: FileSource
    , position :: Position
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

instance Pretty Location where
    pretty Location{file, position} =
        Pretty.hsep [pretty file, ": ", pretty (position.line, position.column)]

data Position = Position
    { line :: Int
    , column :: Int
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

data Constrained = Concrete | Symbolic
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

type VarnameAndSort = (ByteString, ByteString)

data Concreteness
    = Unconstrained
    | AllConstrained Constrained
    | SomeConstrained (Map VarnameAndSort Constrained)
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

data FunctionType = Partial | Total
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data SymbolType
    = Function FunctionType
    | Constructor
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

newtype SyntacticClauses = SyntacticClauses [Word8]
    deriving stock (Eq, Ord, Read, Show)
    deriving newtype (NFData)

pattern IsIdem, IsNotIdem :: Flag "isIdem"
pattern IsIdem = Flag True
pattern IsNotIdem = Flag False
{-# COMPLETE IsIdem, IsNotIdem #-}

pattern IsAssoc, IsNotAssoc :: Flag "isAssoc"
pattern IsAssoc = Flag True
pattern IsNotAssoc = Flag False
{-# COMPLETE IsAssoc, IsNotAssoc #-}

pattern IsMacroOrAlias, IsNotMacroOrAlias :: Flag "isMacroOrAlias"
pattern IsMacroOrAlias = Flag True
pattern IsNotMacroOrAlias = Flag False
{-# COMPLETE IsMacroOrAlias, IsNotMacroOrAlias #-}

pattern CanBeEvaluated, CannotBeEvaluated :: Flag "canBeEvaluated"
pattern CanBeEvaluated = Flag True
pattern CannotBeEvaluated = Flag False
{-# COMPLETE CanBeEvaluated, CannotBeEvaluated #-}

data SMTType
    = SMTLib ByteString
    | SMTHook SExpr
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data SymbolAttributes = SymbolAttributes
    { symbolType :: SymbolType
    , isIdem :: Flag "isIdem"
    , isAssoc :: Flag "isAssoc"
    , isMacroOrAlias :: Flag "isMacroOrAlias"
    , hasEvaluators :: Flag "canBeEvaluated"
    , collectionMetadata :: Maybe KCollectionMetadata
    , smt :: Maybe SMTType
    , hook :: Maybe ByteString
    }
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data KCollectionTag = KMapTag | KListTag | KSetTag
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data KCollectionMetadata
    = KMapMeta KMapDefinition
    | KListMeta KListDefinition
    | KSetMeta KSetDefinition
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data KCollectionSymbolNames = KCollectionSymbolNames
    { unitSymbolName :: ByteString
    , elementSymbolName :: ByteString
    , concatSymbolName :: ByteString
    }
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data KMapDefinition = KMapDefinition
    { symbolNames :: KCollectionSymbolNames
    , keySortName :: ByteString
    , elementSortName :: ByteString
    , mapSortName :: ByteString
    }
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

data KListDefinition = KListDefinition
    { symbolNames :: KCollectionSymbolNames
    , elementSortName :: ByteString
    , listSortName :: ByteString
    }
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

type KSetDefinition = KListDefinition

data SortAttributes = SortAttributes
    { argCount :: Int
    , collectionAttributes :: Maybe (KCollectionSymbolNames, KCollectionTag)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)
