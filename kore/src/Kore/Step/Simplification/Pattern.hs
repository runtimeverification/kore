{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

-}
module Kore.Step.Simplification.Pattern
    ( simplifyAndRemoveTopExists
    , simplify
    ) where

import Branch
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrPattern
    ( OrPattern
    )
import Kore.Internal.Pattern
    ( Conditional (..)
    , Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.TermLike
    ( pattern Exists_
    )
import Kore.Logger
    ( LogMessage
    , WithLog
    )
import qualified Kore.Step.Merging.Pattern as Pattern
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    , SimplifierVariable
    , simplifyTerm
    )

simplifyAndRemoveTopExists
    ::  ( SimplifierVariable variable
        , MonadSimplify simplifier
        , WithLog LogMessage simplifier
        )
    => Pattern variable
    -> simplifier (OrPattern variable)
simplifyAndRemoveTopExists patt = do
    simplified <- simplify patt
    return (removeTopExists <$> simplified)
  where
    removeTopExists :: Pattern variable -> Pattern variable
    removeTopExists p@Conditional{ term = Exists_ _ _ quantified } =
        removeTopExists p {term = quantified}
    removeTopExists p = p

{-| Simplifies an 'Pattern', returning an 'OrPattern'.
-}
simplify
    ::  ( SimplifierVariable variable
        , MonadSimplify simplifier
        , WithLog LogMessage simplifier
        )
    => Pattern variable
    -> simplifier (OrPattern variable)
simplify pattern'@Conditional { term } = do
    simplifiedTerm <- simplifyTerm term
    orPatterns <-
        Branch.gather
        $ traverse
            (Pattern.mergeWithPredicate $ Pattern.withoutTerm pattern')
            simplifiedTerm
    return (MultiOr.mergeAll orPatterns)
