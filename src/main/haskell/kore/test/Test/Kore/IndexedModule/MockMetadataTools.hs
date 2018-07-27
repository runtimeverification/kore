module Test.Kore.IndexedModule.MockMetadataTools
    ( makeMetadataTools
    , makeSortTools
    , constructorAttributes
    , defaultAttributes
    , functionAttributes
    ) where

import Data.Default
       ( def )
import Data.Maybe
       ( fromMaybe )

import           Kore.AST.Common
                 ( SymbolOrAlias (..) )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (MetadataTools), SortTools )
import qualified Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes (StepperAttributes) )
import qualified Kore.Step.StepperAttributes as StepperAttributes
                 ( StepperAttributes (..) )

makeMetadataTools
    :: SortTools level
    -> [(SymbolOrAlias level, StepperAttributes)]
    -> MetadataTools level StepperAttributes
makeMetadataTools sortTools attr =
    MetadataTools
        { attributes = attributesFunction attr
        , sortTools = sortTools
        }

makeSortTools
    :: [(SymbolOrAlias level, ApplicationSorts level)]
    -> SymbolOrAlias level -> ApplicationSorts level
makeSortTools = caseBasedFunction

attributesFunction
    :: [(SymbolOrAlias level, StepperAttributes)]
    -> SymbolOrAlias level
    -> StepperAttributes
attributesFunction = caseBasedFunction

caseBasedFunction
    :: (Eq a, Show a)
    => [(a, b)] -> a -> b
caseBasedFunction cases arg =
    fromMaybe
        (error ("Unknown argument: " ++ show arg))
        (lookup arg cases)

functionAttributes :: StepperAttributes
functionAttributes = StepperAttributes
    { isConstructor = False
    , isFunctional = False
    , isFunction = True
    , hook = def
    }

constructorAttributes :: StepperAttributes
constructorAttributes = StepperAttributes
    { isConstructor = True
    , isFunctional = True
    , isFunction = False
    , hook = def
    }

defaultAttributes :: StepperAttributes
defaultAttributes = StepperAttributes
    { isConstructor = False
    , isFunctional = False
    , isFunction = False
    , hook = def
    }
