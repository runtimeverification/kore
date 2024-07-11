{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Log.DebugCreatedSubstitution (
    DebugCreatedSubstitution,
    debugCreatedSubstitution,
) where

import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Substitution (Substitution)
import Kore.Internal.Substitution qualified as Substitution
import Kore.Rewrite.RewritingVariable (RewritingVariableName)
import Kore.Sort (Sort)
import Kore.Unparser (layoutPrettyUnbounded, unparse)
import Log
import Prelude.Kore
import Pretty (Pretty (..))
import Pretty qualified

data DebugCreatedSubstitution = DebugCreatedSubstitution
    { substitution :: Substitution RewritingVariableName
    , resultSort :: Sort
    }
    deriving stock (Show)

instance Pretty DebugCreatedSubstitution where
    pretty (DebugCreatedSubstitution{substitution, resultSort}) =
        Pretty.vsep
            [ "Created and applied the following substitution:"
            , Pretty.indent 4 (unparse substitutionTerm)
            ]
      where
        substitutionTerm =
            Predicate.fromPredicate resultSort
                . Substitution.toPredicate
                $ substitution

instance Entry DebugCreatedSubstitution where
    entrySeverity _ = Debug
    oneLineDoc =
        pretty
            . Pretty.renderString
            . layoutPrettyUnbounded
            . Pretty.group
            . pretty
    oneLineContextDoc _ = single CtxSubstitution
    helpDoc _ = "log every substitution created when applying semantic rules"

debugCreatedSubstitution ::
    MonadLog log =>
    Substitution RewritingVariableName ->
    Sort ->
    log ()
debugCreatedSubstitution substitution resultSort =
    logEntry (DebugCreatedSubstitution{substitution, resultSort})
