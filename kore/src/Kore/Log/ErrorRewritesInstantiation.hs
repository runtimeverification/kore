{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.ErrorRewritesInstantiation (
    ErrorRewritesInstantiation (..),
    checkSubstitutionCoverage,
) where

import Control.Exception (
    Exception (..),
    throw,
 )
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import GHC.Exception (
    prettyCallStackLines,
 )
import GHC.Generics qualified as GHC
import GHC.Stack (
    CallStack,
    callStack,
 )
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom (
    SourceLocation,
 )
import Kore.Internal.Conditional (
    Conditional (..),
 )
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.TermLike (
    isConstructorLike,
 )
import Kore.Internal.Variable (
    SomeVariableName,
 )
import Kore.Rewrite.AxiomPattern (
    AxiomPattern,
    getAxiomPattern,
 )
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.Step (
    UnifiedRule,
    UnifyingRule (..),
    wouldNarrowWith,
 )
import Kore.Unparser (
    unparse,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

data ErrorRewritesInstantiation = ErrorRewritesInstantiation
    { problem :: !SubstitutionCoverageError
    , configuration :: !(Pattern RewritingVariableName)
    , errorCallStack :: !CallStack
    }
    deriving stock (Show, GHC.Generic)

data SubstitutionCoverageError = SubstitutionCoverageError
    { solution ::
        !( Conditional
            RewritingVariableName
            (AxiomPattern RewritingVariableName)
         )
    , location :: !SourceLocation
    , missingVariables :: !(Set (SomeVariableName RewritingVariableName))
    }
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Exception ErrorRewritesInstantiation where
    toException = toException . SomeEntry []
    fromException exn =
        fromException exn >>= fromEntry

instance Entry ErrorRewritesInstantiation where
    entrySeverity _ = Error
    oneLineDoc
        ErrorRewritesInstantiation
            { problem = SubstitutionCoverageError{location}
            } =
            pretty location
    oneLineContextDoc _ = single CtxError
    helpDoc _ = "log rewrite instantiation errors"

instance Pretty ErrorRewritesInstantiation where
    pretty
        ErrorRewritesInstantiation
            { problem =
                SubstitutionCoverageError{solution, location, missingVariables}
            , configuration
            , errorCallStack
            } =
            Pretty.vsep $
                [ "While rewriting the configuration:"
                , Pretty.indent 4 (unparse configuration)
                , "Unable to instantiate semantic rule at "
                    <> Pretty.pretty location
                , "Unification did not find a solution for the variables:"
                , (Pretty.indent 4 . Pretty.sep)
                    (unparse <$> Set.toAscList missingVariables)
                , "The unification solution was:"
                , unparse (fmap getAxiomPattern solution)
                , "Error! Please report this."
                ]
                    <> fmap Pretty.pretty (prettyCallStackLines errorCallStack)

{- | Check that the final substitution covers the applied rule appropriately.

For normal execution, the final substitution should cover all the free
variables on the left-hand side of the applied rule; otherwise, we would
wrongly introduce existentially-quantified variables into the final
configuration. Failure of the coverage check indicates a problem with
unification, so in that case @checkSubstitutionCoverage@ throws
an error message with the axiom and the initial and final configurations.

For symbolic execution, we expect to replace symbolic variables with
more specific patterns (narrowing), so we just quantify the variables
we added to the result.

@checkSubstitutionCoverage@ calls @quantifyVariables@ to remove
the axiom variables from the substitution and unwrap all the 'Target's.
-}
checkSubstitutionCoverage ::
    forall rule monadLog.
    MonadLog monadLog =>
    UnifyingRule rule =>
    From rule SourceLocation =>
    From rule (AxiomPattern RewritingVariableName) =>
    UnifyingRuleVariable rule ~ RewritingVariableName =>
    HasCallStack =>
    -- | Initial configuration
    Pattern RewritingVariableName ->
    -- | Unified rule
    UnifiedRule rule ->
    monadLog ()
checkSubstitutionCoverage configuration unifiedRule
    | isCoveringSubstitution || isSymbolic = return ()
    | otherwise =
        -- The substitution does not cover all the variables on the left-hand side
        -- of the rule *and* we did not generate a substitution for a symbolic
        -- initial configuration. This is a fatal error because it indicates
        -- something has gone horribly wrong.
        throw
            ErrorRewritesInstantiation
                { problem = substitutionCoverageError
                , configuration
                , errorCallStack = callStack
                }
  where
    ~substitutionCoverageError =
        SubstitutionCoverageError{solution, location, missingVariables}

    missingVariables = wouldNarrowWith unifiedRule
    isCoveringSubstitution = Set.null missingVariables
    location = from @_ @SourceLocation . term $ unifiedRule
    {- This is lazy because it may call allPathRuleToTerm or onePathRuleToTerm,
       which may end up calling illSorted
    -}
    ~solution = from @rule @(AxiomPattern _) <$> unifiedRule
    isSymbolic = (not . isConstructorLike) (term configuration)
