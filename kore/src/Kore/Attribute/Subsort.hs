{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Subsort (
    Subsort (..),
    Subsorts (..),
    subsortId,
    subsortSymbol,
    subsortAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | The @subsort@ attribute.
data Subsort = Subsort
    { subsort :: Sort
    , supersort :: Sort
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

newtype Subsorts = Subsorts {getSubsorts :: [Subsort]}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Subsorts where
    def = Subsorts []

-- | Kore identifier representing a @subsort@ attribute symbol.
subsortId :: Id
subsortId = "subsort"

{- | Kore symbol representing a @subsort@ attribute head.

Kore syntax: @subsort{Sub,Super}@
where @Sub@ is the subsort and @Super@ is the supersort.
-}
subsortSymbol :: Sort -> Sort -> SymbolOrAlias
subsortSymbol subsort supersort =
    SymbolOrAlias
        { symbolOrAliasConstructor = subsortId
        , symbolOrAliasParams = [subsort, supersort]
        }

{- | Kore pattern representing a @subsort@ attribute.

Kore syntax: @subsort{Sub,Super}()@
where @Sub@ is the subsort and @Super@ is the supersort.
-}
subsortAttribute :: Sort -> Sort -> AttributePattern
subsortAttribute subsort supersort =
    attributePattern_ $ subsortSymbol subsort supersort

{- | Parse @subsort@ Kore attributes, if present.

  It is a parse error if the @subsort@ attribute is not given exactly
  two sort parameters

  See also: 'subsortAttribute'
-}
instance ParseAttributes Subsorts where
    parseAttribute =
        withApplication' $ \params args (Subsorts subsorts) -> do
            Parser.getZeroArguments args
            (subsort, supersort) <- Parser.getTwoParams params
            let relation = Subsort subsort supersort
            return (Subsorts $ relation : subsorts)
      where
        withApplication' = Parser.withApplication subsortId

instance From Subsorts Attributes where
    from =
        Attributes . map toAttribute . getSubsorts
      where
        toAttribute = subsortAttribute <$> subsort <*> supersort
