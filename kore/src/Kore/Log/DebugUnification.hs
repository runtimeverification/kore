{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugUnification (
    DebugUnification (..),
    WhileDebugUnification (..),
    UnificationSolved (..),
    UnificationUnsolved (..),
    debugUnificationSolved,
    debugUnificationUnsolved,
    whileDebugUnification,
) where

import Kore.Internal.Pattern as Pattern
import Kore.Internal.TermLike as TermLike
import Kore.Unparser
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

data DebugUnification
    = DebugUnificationWhile WhileDebugUnification
    | DebugUnificationSolved UnificationSolved
    | DebugUnificationUnsolved UnificationUnsolved
    deriving stock (Show)

instance Pretty DebugUnification where
    pretty (DebugUnificationWhile x) = Pretty.pretty x
    pretty (DebugUnificationSolved x) = Pretty.pretty x
    pretty (DebugUnificationUnsolved x) = Pretty.pretty x

instance Entry DebugUnification where
    entrySeverity _ = Debug
    oneLineDoc (DebugUnificationWhile _) = "DebugUnificationWhile"
    oneLineDoc (DebugUnificationSolved _) = "DebugUnificationSolved"
    oneLineDoc (DebugUnificationUnsolved _) = "DebugUnificationUnsolved"
    oneLineContextDoc _ = single CtxUnify

-- | @WhileDebugUnification@ encloses the context of unification log entries.
data WhileDebugUnification = WhileDebugUnification {term1, term2 :: TermLike VariableName}
    deriving stock (Show)

instance Pretty WhileDebugUnification where
    pretty WhileDebugUnification{term1, term2} =
        Pretty.vsep
            [ "Unifying terms:"
            , Pretty.indent 4 (unparse term1)
            , Pretty.indent 2 "and:"
            , Pretty.indent 4 (unparse term2)
            ]

-- | @UnificationUnsolved@ represents an unsolved unification problem.
data UnificationUnsolved = UnificationUnsolved {term1, term2 :: TermLike VariableName}
    deriving stock (Show)

instance Pretty UnificationUnsolved where
    pretty UnificationUnsolved{term1, term2} =
        Pretty.vsep
            [ "Unification unknown:"
            , Pretty.indent 4 (unparse term1)
            , Pretty.indent 2 "and:"
            , Pretty.indent 4 (unparse term2)
            ]

-- | @UnificationSolved@ represents the solution of a unification problem.
newtype UnificationSolved = UnificationSolved {solution :: Pattern VariableName}
    deriving stock (Show)

instance Pretty UnificationSolved where
    pretty UnificationSolved{solution} =
        Pretty.vsep
            [ "Unification solution:"
            , Pretty.indent 4 (unparse solution)
            ]

whileDebugUnification ::
    MonadLog m =>
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    m a ->
    m a
whileDebugUnification term1' term2' =
    logWhile $ DebugUnificationWhile WhileDebugUnification{term1, term2}
  where
    term1 = TermLike.mapVariables (pure toVariableName) term1'
    term2 = TermLike.mapVariables (pure toVariableName) term2'

debugUnificationSolved ::
    MonadLog m =>
    InternalVariable variable =>
    Pattern variable ->
    m ()
debugUnificationSolved solution' =
    logEntry $ DebugUnificationSolved UnificationSolved{solution}
  where
    solution = Pattern.mapVariables (pure toVariableName) solution'

debugUnificationUnsolved ::
    MonadLog m =>
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    m ()
debugUnificationUnsolved term1' term2' =
    logEntry $ DebugUnificationUnsolved UnificationUnsolved{term1, term2}
  where
    term1 = TermLike.mapVariables (pure toVariableName) term1'
    term2 = TermLike.mapVariables (pure toVariableName) term2'
