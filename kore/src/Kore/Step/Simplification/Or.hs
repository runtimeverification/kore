{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

-}
module Kore.Step.Simplification.Or
    ( simplifyEvaluated
    , simplify
    ) where

import Prelude.Kore ()

import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrPattern
    ( OrPattern
    )
import Kore.Internal.TermLike

-- * Driver

{- | 'simplify' simplifies an 'Or' pattern into an 'OrPattern'.

'simplify' is the driver responsible for breaking down an @\\or@ pattern and
merging its children.

-}
simplify
    :: InternalVariable variable
    => Or Sort (OrPattern variable)
    -> OrPattern variable
simplify Or { orFirst = first, orSecond = second } =
    simplifyEvaluated first second

{- | Simplify an 'Or' given its two 'OrPattern' children.

See also: 'simplify'

-}
simplifyEvaluated
    :: InternalVariable variable
    => OrPattern variable
    -> OrPattern variable
    -> OrPattern variable

{-

__TODO__ (virgil): Preserve pattern sorts under simplification.
One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

@
CofreeF (Or Sort) (Attribute.Pattern variable) (OrPattern variable)
@

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache
information besides the pattern sort, which will make it even more useful to
carry around.

-}

{-
__TODO__ (virgil): This should do all possible mergings, not just the first term
with the second.
-}

simplifyEvaluated first second = MultiOr.merge first second
