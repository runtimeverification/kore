{-|
Module      : Kore.Attribute.Axiom
Description : Axiom sentence attributes
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

-}

module Kore.Attribute.Axiom
    ( Axiom (..)
    , HeatCool (..)
    , ProductionID (..)
    , Priority (..)
    , Owise (..)
    , Assoc (..)
    , Comm (..)
    , Unit (..)
    , Idem (..)
    , Trusted (..)
    , Concrete (..)
    , Symbolic (..)
    , Simplification (..)
    , Overload (..)
    , SmtLemma (..)
    , Label (..)
    , SourceLocation (..)
    , Constructor (..)
    , RuleIndex (..)
    , RuleIndexCase (..)
    , UniqueId (..)
    , PriorityAttributes (..)
    , axiomSymbolToSymbolOrAlias
    , mapAxiomVariables
    , parseAxiomAttributes
    , getPriorityOfAxiom
    ) where

import Prelude.Kore

import Control.DeepSeq
    ( NFData
    )
import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import Data.Default
    ( Default (..)
    )
import qualified Data.Default as Default
import qualified Data.Foldable as Foldable
import Data.Generics.Product
import Data.Proxy
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Assoc
import Kore.Attribute.Attributes
import Kore.Attribute.Axiom.Concrete
import Kore.Attribute.Axiom.Constructor
import Kore.Attribute.Axiom.Symbolic
import Kore.Attribute.Axiom.Unit
import Kore.Attribute.Comm
import Kore.Attribute.Functional
import Kore.Attribute.HeatCool
import Kore.Attribute.Idem
import Kore.Attribute.Label
import Kore.Attribute.Overload
import Kore.Attribute.Owise
import Kore.Attribute.Parser
    ( ParseAttributes (..)
    , Parser
    , SymbolOrAlias
    , toAttributes
    )
import Kore.Attribute.Priority
import Kore.Attribute.ProductionID
import Kore.Attribute.RuleIndex
import Kore.Attribute.Simplification
import Kore.Attribute.SmtLemma
import Kore.Attribute.SourceLocation
import Kore.Attribute.Subsort
import Kore.Attribute.Trusted
import Kore.Attribute.UniqueId
import Kore.Debug
import Kore.Internal.Symbol
    ( Symbol (..)
    , toSymbolOrAlias
    )
import Kore.Syntax.Variable hiding
    ( Concrete
    )
import qualified SQL

{- | Attributes specific to Kore axiom sentences.
 -}
data Axiom symbol variable =
    Axiom
    { heatCool :: !HeatCool
    -- ^ An axiom may be denoted as a heating or cooling rule.
    , productionID :: !ProductionID
    -- ^ The identifier from the front-end identifying a rule or group of rules.
    , priority :: !Priority
    -- ^ A number associated to each rule,
    -- which specifies the order of application
    , assoc :: !Assoc
    -- ^ The axiom is an associativity axiom.
    , comm :: !Comm
    -- ^ The axiom is a commutativity axiom.
    , unit :: !Unit
    -- ^ The axiom is a left- or right-unit axiom.
    , idem :: !Idem
    -- ^ The axiom is an idempotency axiom.
    , trusted :: !Trusted
    -- ^ The claim is trusted
    , concrete :: !(Concrete variable)
    , symbolic :: !(Symbolic variable)
    , simplification :: !Simplification
    -- ^ This is an axiom used for simplification
    -- (as opposed to, e.g., function evaluation).
    , overload :: !(Overload symbol)
    -- ^ The axiom is an overloaded-production axiom.
    , smtLemma :: !SmtLemma
    -- ^ The axiom should be sent to SMT as a lemma.
    , label :: !Label
    -- ^ The user-defined label associated with the axiom.
    , sourceLocation :: !SourceLocation
    -- ^ Source and location in the original file.
    , constructor :: !Constructor
    -- ^ Shows that this is one of the constructor axioms
    -- (e.g. no confusion, no junk)
    , functional :: !Functional
    -- ^ Shows that this is one of the functionality axioms
    , subsorts :: !Subsorts
    -- ^ Shows that this describes a subsorting axiom
    , identifier :: !RuleIndex
    -- ^ Used to identify an axiom in the repl.
    , uniqueId :: !UniqueId
    -- ^ Unique id, usually generated by the frontend on request.
    , owise :: !Owise
    -- ^ This is an owise evaluation rule.
    }
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic (Axiom symbol variable)

instance SOP.HasDatatypeInfo (Axiom symbol variable)

instance (Debug symbol, Debug variable) => Debug (Axiom symbol variable)

instance
    (Debug symbol, Debug variable, Diff symbol, Diff variable)
    => Diff (Axiom symbol variable)

instance (NFData symbol, NFData variable) => NFData (Axiom symbol variable)

instance Default (Axiom symbol variable) where
    def =
        Axiom
            { heatCool = def
            , productionID = def
            , priority = def
            , assoc = def
            , comm = def
            , unit = def
            , idem = def
            , trusted = def
            , concrete = def
            , symbolic = def
            , simplification = def
            , overload = def
            , smtLemma = def
            , label = def
            , sourceLocation = def
            , constructor = def
            , functional = def
            , subsorts = def
            , identifier = def
            , uniqueId = def
            , owise = def
            }

instance
    From symbol SymbolOrAlias
    => From (Axiom symbol VariableName) Attributes
  where
    from =
        mconcat . sequence
            [ from . heatCool
            , from . productionID
            , from . priority
            , from . assoc
            , from . comm
            , from . unit
            , from . idem
            , from . trusted
            , from . concrete
            , from . symbolic
            , from . simplification
            , from . overload
            , from . smtLemma
            , from . label
            , from . sourceLocation
            , from . constructor
            , from . functional
            , from . subsorts
            , from . uniqueId
            , from . owise
            ]

instance From (Axiom symbol variable) PriorityAttributes where
    from Axiom { priority, owise, simplification } =
        PriorityAttributes
            { priorityAttr = priority
            , owiseAttr = owise
            , simplificationAttr = simplification
            }

instance From (Axiom symbol variable) HeatCool where
    from Axiom { heatCool } = heatCool

instance SQL.Column (Axiom SymbolOrAlias VariableName) where
    -- TODO (thomas.tuegel): Use a foreign key.
    defineColumn tableName _ =
        SQL.defineColumn tableName (Proxy @AttributePattern)
    toColumn = SQL.toColumn . toAttributes

instance SQL.Column (Axiom Symbol VariableName) where
    -- TODO (thomas.tuegel): Use a foreign key.
    defineColumn tableName _ =
        SQL.defineColumn tableName (Proxy @AttributePattern)
    toColumn = SQL.toColumn . toAttributes

axiomSymbolToSymbolOrAlias
    :: Axiom Symbol variable
    -> Axiom SymbolOrAlias variable
axiomSymbolToSymbolOrAlias axiom =
    axiom & field @"overload" Lens.%~ fmap toSymbolOrAlias

parseAxiomAttribute
    :: FreeVariables VariableName
    -> AttributePattern
    -> Axiom SymbolOrAlias VariableName
    -> Parser (Axiom SymbolOrAlias VariableName)
parseAxiomAttribute freeVariables attr =
        typed @HeatCool (parseAttribute attr)
        Monad.>=> typed @ProductionID (parseAttribute attr)
        Monad.>=> typed @Priority (parseAttribute attr)
        Monad.>=> typed @Assoc (parseAttribute attr)
        Monad.>=> typed @Comm (parseAttribute attr)
        Monad.>=> typed @Unit (parseAttribute attr)
        Monad.>=> typed @Idem (parseAttribute attr)
        Monad.>=> typed @Trusted (parseAttribute attr)
        Monad.>=>
            typed @(Concrete VariableName)
                (parseConcreteAttribute freeVariables attr)
        Monad.>=>
            typed @(Symbolic VariableName)
                (parseSymbolicAttribute freeVariables attr)
        Monad.>=> typed @Simplification (parseAttribute attr)
        Monad.>=> typed @(Overload SymbolOrAlias) (parseAttribute attr)
        Monad.>=> typed @SmtLemma (parseAttribute attr)
        Monad.>=> typed @Label (parseAttribute attr)
        Monad.>=> typed @SourceLocation (parseAttribute attr)
        Monad.>=> typed @Constructor (parseAttribute attr)
        Monad.>=> typed @Functional (parseAttribute attr)
        Monad.>=> typed @Subsorts (parseAttribute attr)
        Monad.>=> typed @UniqueId (parseAttribute attr)
        Monad.>=> typed @Owise (parseAttribute attr)

parseAxiomAttributes
    :: FreeVariables VariableName
    -> Attributes
    -> Parser (Axiom SymbolOrAlias VariableName)
parseAxiomAttributes freeVariables (Attributes attrs) =
    Foldable.foldlM (flip $ parseAxiomAttribute freeVariables) Default.def attrs

mapAxiomVariables
    :: Ord variable2
    => AdjSomeVariableName (variable1 -> variable2)
    -> Axiom symbol variable1
    -> Axiom symbol variable2
mapAxiomVariables adj axiom@Axiom { concrete, symbolic } =
    axiom
        { concrete = mapConcreteVariables adj concrete
        , symbolic = mapSymbolicVariables adj symbolic
        }

data PriorityAttributes =
    PriorityAttributes
    { priorityAttr :: !Priority
    , owiseAttr :: !Owise
    , simplificationAttr :: !Simplification
    }

getPriorityOfAxiom
    :: forall attrs
    .  HasCallStack
    => From attrs PriorityAttributes
    => attrs
    -> Integer
getPriorityOfAxiom
    (from @attrs ->
        PriorityAttributes
            { priorityAttr
            , owiseAttr
            , simplificationAttr
            }
    )
  =
    case (priorityAttr, owiseAttr, simplificationAttr) of
        (Priority Nothing, Owise True, NotSimplification) ->
            owisePriority
        (Priority Nothing, Owise False, NotSimplification) ->
            defaultPriority
        (Priority (Just value), Owise False, NotSimplification) ->
            value
        (Priority Nothing, Owise False, IsSimplification Nothing) ->
            defaultSimplificationPriority
        (Priority Nothing, Owise False, IsSimplification (Just value)) ->
            value
        -- TODO: remove this case once the frontend
        -- modifies the simplification attribute
        -- to take an optional priority value
        (Priority (Just value), Owise False, IsSimplification Nothing) ->
            value
        errorCase@(_, _, _) ->
            error
                ("An axiom cannot have the following \
                \ combination of attributes: "
                <> show errorCase
                <> " Please report this error."
                )

