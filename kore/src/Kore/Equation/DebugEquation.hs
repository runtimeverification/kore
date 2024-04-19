{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Equation.DebugEquation (
    AttemptEquationResult,

    -- * Errors
    AttemptEquationError (..),
    MatchError (..),
    ApplyMatchResultErrors (..),
    ApplyMatchResultError (..),
    CheckRequiresError (..),
    whileCheckRequires,
    whileMatch,
    whileApplyMatchResult,
    whileDebugAttemptEquation,

    -- * Logging
    DebugAttemptEquation (..),
    DebugApplyEquation (..),
    debugApplyEquation,
    debugAttemptEquationResult,
    srcLoc,
) where

import Control.Error (
    ExceptT,
    withExceptT,
 )
import Data.Aeson qualified as JSON
import Data.Text (
    Text,
 )
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Source qualified as Attribute
import Kore.Attribute.SourceLocation (
    SourceLocation (..),
 )
import Kore.Equation.Equation (Equation (..))
import Kore.Internal.OrCondition (OrCondition)
import Kore.Internal.Pattern (Pattern)
import Kore.Internal.Predicate (Predicate)
import Kore.Internal.SideCondition (SideCondition)
import Kore.Internal.TermLike (
    AstLocation (AstLocationFile),
    FileLocation,
    SomeVariableName,
    TermLike,
 )
import Kore.Rewrite.Axiom.MatcherData (
    MatchResult,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Unparser (Unparse (..))
import Log
import Prelude.Kore
import Pretty (Pretty (..))
import Pretty qualified

{- | The outcome of an attempt to apply an 'Equation'.

@AttemptEquationResult@ is 'Right' if the equation is applicable, and 'Left'
otherwise. If the equation is not applicable, the 'AttemptEquationError' will
indicate the reason.
-}
type AttemptEquationResult variable =
    Either (AttemptEquationError variable) (Pattern variable)

-- * Errors

-- | Errors that can occur during 'attemptEquation'.
data AttemptEquationError variable
    = WhileMatch !(MatchError variable)
    | WhileApplyMatchResult !(ApplyMatchResultErrors variable)
    | WhileCheckRequires !(CheckRequiresError variable)
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

whileMatch ::
    Functor monad =>
    ExceptT (MatchError RewritingVariableName) monad a ->
    ExceptT (AttemptEquationError RewritingVariableName) monad a
whileMatch = withExceptT WhileMatch

whileApplyMatchResult ::
    Functor monad =>
    ExceptT (ApplyMatchResultErrors RewritingVariableName) monad a ->
    ExceptT (AttemptEquationError RewritingVariableName) monad a
whileApplyMatchResult = withExceptT WhileApplyMatchResult

whileCheckRequires ::
    Functor monad =>
    ExceptT (CheckRequiresError RewritingVariableName) monad a ->
    ExceptT (AttemptEquationError RewritingVariableName) monad a
whileCheckRequires = withExceptT WhileCheckRequires

instance Pretty (AttemptEquationError RewritingVariableName) where
    pretty (WhileMatch matchError) =
        pretty matchError
    pretty (WhileApplyMatchResult applyMatchResultErrors) =
        pretty applyMatchResultErrors
    pretty (WhileCheckRequires checkRequiresError) =
        pretty checkRequiresError

-- | Errors that can occur while matching the equation to the term.
data MatchError variable = MatchError
    { matchTerm :: !(TermLike variable)
    , matchEquation :: !(Equation variable)
    , matchFailReason :: !Text
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Pretty (MatchError RewritingVariableName) where
    pretty MatchError{matchFailReason} =
        Pretty.hsep ["equation did not match term: ", pretty matchFailReason]

{- | Errors that can occur during 'applyMatchResult'.

There may be multiple independent reasons the match cannot be applied, so this
type contains a 'NonEmpty' list of 'ApplyMatchError'.
-}
data ApplyMatchResultErrors variable = ApplyMatchResultErrors
    { matchResult :: !(MatchResult variable)
    , applyMatchErrors :: !(NonEmpty (ApplyMatchResultError variable))
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Pretty (ApplyMatchResultErrors RewritingVariableName) where
    pretty ApplyMatchResultErrors{applyMatchErrors} =
        Pretty.vsep
            [ "could not apply match result:"
            , (Pretty.indent 4 . Pretty.vsep)
                (pretty <$> toList applyMatchErrors)
            ]

-- | @ApplyMatchResultError@ represents a reason the match could not be applied.
data ApplyMatchResultError variable
    = -- | The variable was matched with a symbolic term where a concrete
      -- term was required.
      NotConcrete (SomeVariableName variable) (TermLike variable)
    | -- | The variable was matched with a concrete term where a symbolic
      -- term was required.
      NotSymbolic (SomeVariableName variable) (TermLike variable)
    | -- | The variable was not matched.
      NotMatched (SomeVariableName variable)
    | -- | The variable is not part of the matching solution.
      NonMatchingSubstitution (SomeVariableName variable)
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Pretty (ApplyMatchResultError RewritingVariableName) where
    pretty (NotConcrete variable _) =
        Pretty.hsep
            [ "variable"
            , unparse variable
            , "did not match a concrete term"
            ]
    pretty (NotSymbolic variable _) =
        Pretty.hsep
            [ "variable"
            , unparse variable
            , "did not match a symbolic term"
            ]
    pretty (NotMatched variable) =
        Pretty.hsep ["variable", unparse variable, "was not matched"]
    pretty (NonMatchingSubstitution variable) =
        Pretty.hsep
            [ "variable"
            , unparse variable
            , "should not be substituted"
            ]

-- | Errors that can occur during 'checkRequires'.
data CheckRequiresError variable = CheckRequiresError
    { matchPredicate :: !(Predicate variable)
    , equationRequires :: !(Predicate variable)
    , sideCondition :: !(SideCondition variable)
    , negatedImplication :: !(OrCondition variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Pretty (CheckRequiresError RewritingVariableName) where
    pretty checkRequiresError =
        Pretty.vsep
            [ "Could not infer the equation requirement:"
            , Pretty.indent 4 (pretty equationRequires)
            , "and the matching requirement:"
            , Pretty.indent 4 (pretty matchPredicate)
            , "from the side condition:"
            , Pretty.indent 4 (pretty sideCondition)
            , "The negated implication is:"
            , Pretty.indent 4 (pretty negatedImplication)
            ]
      where
        CheckRequiresError
            { matchPredicate
            , equationRequires
            , sideCondition
            , negatedImplication
            } = checkRequiresError

-- * Logging

-- | Log entries for all phases of equation application.
data DebugAttemptEquation
    = -- | Covers the entire scope of 'attemptEquation'.
      DebugAttemptEquation
        (Equation RewritingVariableName)
        (TermLike RewritingVariableName)
    | -- | Entered into the log when an equation is applicable.
      DebugAttemptEquationResult
        (Equation RewritingVariableName)
        (AttemptEquationResult RewritingVariableName)
    deriving stock (Show)
    deriving stock (GHC.Generic)

instance Pretty DebugAttemptEquation where
    pretty (DebugAttemptEquation equation termLike) =
        Pretty.vsep
            [ (Pretty.hsep . catMaybes)
                [ Just "applying equation"
                , (\label -> Pretty.hsep ["(label: ", pretty label, ")"]) <$> ruleLabel equation
                , (\loc -> Pretty.hsep ["at", pretty loc]) <$> srcLoc equation
                , Just "to term:"
                ]
            , Pretty.indent 4 (unparse termLike)
            ]
    pretty (DebugAttemptEquationResult _ (Left attemptEquationError)) =
        Pretty.vsep
            [ "equation is not applicable:"
            , pretty attemptEquationError
            ]
    pretty (DebugAttemptEquationResult _ (Right _)) =
        "equation is applicable"

instance Entry DebugAttemptEquation where
    entrySeverity _ = Debug
    contextDoc (DebugAttemptEquation equation _) =
        (Just . Pretty.hsep . catMaybes)
            [ Just "while applying equation"
            , (\label -> Pretty.hsep ["(label: ", pretty label, ")"]) <$> ruleLabel equation
            , (\loc -> Pretty.hsep ["at", pretty loc]) <$> srcLoc equation
            ]
    contextDoc _ = Nothing
    helpDoc _ = "log equation application attempts"
    oneLineDoc (DebugAttemptEquation equation _) =
        maybe
            mempty
            (\loc -> Pretty.hsep ["applying equation at", pretty loc])
            (srcLoc equation)
    oneLineDoc (DebugAttemptEquationResult _ (Left _)) = "equation is not applicable"
    oneLineDoc (DebugAttemptEquationResult _ (Right _)) = "equation is applicable"
    oneLineJson entry =
        JSON.object ["entry" JSON..= Log.entryTypeText (Log.toEntry entry)]

-- | Log the result of attempting to apply an 'Equation'.
debugAttemptEquationResult ::
    MonadLog log =>
    Equation RewritingVariableName ->
    AttemptEquationResult RewritingVariableName ->
    log ()
debugAttemptEquationResult equation result =
    logEntry $ DebugAttemptEquationResult equation result

whileDebugAttemptEquation ::
    MonadLog log =>
    TermLike RewritingVariableName ->
    Equation RewritingVariableName ->
    log a ->
    log a
whileDebugAttemptEquation termLike equation =
    logWhile (DebugAttemptEquation equation termLike)

-- | Log when an 'Equation' is actually applied.
data DebugApplyEquation
    = -- | Entered into the log when an equation's result is actually used.
      DebugApplyEquation
        (Equation RewritingVariableName)
        (Pattern RewritingVariableName)
    deriving stock (Show)
    deriving stock (GHC.Generic)

instance Pretty DebugApplyEquation where
    pretty (DebugApplyEquation equation result) =
        Pretty.vsep
            [ (Pretty.hsep . catMaybes)
                [ Just "applied equation"
                , (\label -> Pretty.hsep ["(label: ", pretty label, ")"]) <$> ruleLabel equation
                , (\loc -> Pretty.hsep ["at", pretty loc]) <$> srcLoc equation
                , Just "with result:"
                ]
            , Pretty.indent 4 (unparse result)
            ]

srcLoc :: Equation RewritingVariableName -> Maybe Attribute.SourceLocation
srcLoc equation
    | (not . isLocEmpty) kLoc = Just kLoc
    | AstLocationFile fileLocation <- locationFromAst equation =
        Just (from @FileLocation fileLocation)
    | otherwise = Nothing
  where
    kLoc = Attribute.sourceLocation $ attributes equation

isLocEmpty :: Attribute.SourceLocation -> Bool
isLocEmpty Attribute.SourceLocation{source = Attribute.Source file} =
    isNothing file

ruleLabel :: Equation RewritingVariableName -> Maybe Text
ruleLabel = Attribute.unLabel . Attribute.label . attributes

instance Entry DebugApplyEquation where
    entrySeverity _ = Debug
    oneLineDoc
        ( DebugApplyEquation
                Equation{attributes = Attribute.Axiom{sourceLocation}}
                _
            ) =
            pretty sourceLocation
    oneLineJson entry =
        JSON.object ["entry" JSON..= Log.entryTypeText (Log.toEntry entry)]
    helpDoc _ = "log equation application successes"

{- | Log when an 'Equation' is actually applied.

@debugApplyEquation@ is different from 'debugAttemptEquationResult', which
only indicates if an equation is applicable, that is: if it could apply. If
multiple equations are applicable in the same place, the caller will determine
which is actually applied. Therefore, the /caller/ should use this log entry
after 'attemptEquation'.
-}
debugApplyEquation ::
    MonadLog log =>
    Equation RewritingVariableName ->
    Pattern RewritingVariableName ->
    log ()
debugApplyEquation equation result =
    logEntry $ DebugApplyEquation equation result
