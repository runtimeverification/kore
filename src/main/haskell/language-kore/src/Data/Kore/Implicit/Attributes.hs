{-|
Module      : Data.Kore.Implicit.Attributes
Description : Haskell definitions for the implicit constructs for attributes.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}

module Data.Kore.Implicit.Attributes
    ( attributeObjectSort
    , hookAttribute
    , uncheckedAttributesModule
    ) where

import           Data.Kore.AST.Common
import           Data.Kore.AST.Kore

noParameterObjectSortAndSentence :: String -> (Sort Object, KoreSentence)
noParameterObjectSortAndSentence name =
    ( SortActualSort SortActual
        { sortActualName = Id name
        , sortActualSorts = []
        }
    , ObjectSentence
        ( SentenceSortSentence SentenceSort
            { sentenceSortName       = Id name
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes []
            }
        )
    )

attributeObjectSort :: Sort Object
attributeObjectSortSentence :: KoreSentence
(attributeObjectSort, attributeObjectSortSentence) =
    noParameterObjectSortAndSentence "Attribute"


hookObjectSort :: Sort Object
hookObjectSortSentence :: KoreSentence
(hookObjectSort, hookObjectSortSentence) =
    noParameterObjectSortAndSentence "Hook"

hookName :: String
hookName = "hook"

hookObjectSymbolSentence :: KoreSentence
hookObjectSymbolSentence =
    ObjectSentence
        ( SentenceSymbolSentence SentenceSymbol
            { sentenceSymbolSymbol     = Symbol
                { symbolConstructor = Id hookName
                , symbolParams      = []
                }
            , sentenceSymbolSorts      = [hookObjectSort]
            , sentenceSymbolResultSort = attributeObjectSort
            , sentenceSymbolAttributes = Attributes []
            }
        )

hookAttribute :: String -> UnifiedPattern
hookAttribute hook =
    ObjectPattern
        ( ApplicationPattern Application
            { applicationSymbolOrAlias = SymbolOrAlias
                { symbolOrAliasConstructor = Id hookName
                , symbolOrAliasParams      = []
                }
            , applicationChildren      =
                [ ObjectPattern
                    ( DomainValuePattern DomainValue
                        { domainValueSort  = hookObjectSort
                        , domainValueChild =
                            MetaPattern
                                (StringLiteralPattern (StringLiteral hook))
                        }
                    )
                ]
            }
        )

argumentPositionObjectSort :: Sort Object
argumentPositionObjectSortSentence :: KoreSentence
(argumentPositionObjectSort, argumentPositionObjectSortSentence) =
    noParameterObjectSortAndSentence "ArgumentPosition"

firstArgumentObjectSymbolSentence :: KoreSentence
firstArgumentObjectSymbolSentence =
    noArgumentOrParameterSentence "firstArgument" argumentPositionObjectSort

secondArgumentObjectSymbolSentence :: KoreSentence
secondArgumentObjectSymbolSentence =
    noArgumentOrParameterSentence "secondArgument" argumentPositionObjectSort

thirdArgumentObjectSymbolSentence :: KoreSentence
thirdArgumentObjectSymbolSentence =
    noArgumentOrParameterSentence "thirdArgument" argumentPositionObjectSort

fourthArgumentObjectSymbolSentence :: KoreSentence
fourthArgumentObjectSymbolSentence =
    noArgumentOrParameterSentence "fourthArgument" argumentPositionObjectSort

fifthArgumentObjectSymbolSentence :: KoreSentence
fifthArgumentObjectSymbolSentence =
    noArgumentOrParameterSentence "fifthArgument" argumentPositionObjectSort

strictObjectSort :: Sort Object
strictObjectSortSentence :: KoreSentence
(strictObjectSort, strictObjectSortSentence) =
    noParameterObjectSortAndSentence "Strict"

strictObjectSymbolSentence :: KoreSentence
strictObjectSymbolSentence =
    ObjectSentence
        ( SentenceSymbolSentence SentenceSymbol
            { sentenceSymbolSymbol     = Symbol
                { symbolConstructor = Id "strict"
                , symbolParams      = []
                }
            , sentenceSymbolSorts      =
                [ strictObjectSort
                , argumentPositionObjectSort
                ]
            , sentenceSymbolResultSort = attributeObjectSort
            , sentenceSymbolAttributes = Attributes []
            }
        )

noArgumentOrParameterSentence :: String -> Sort Object -> KoreSentence
noArgumentOrParameterSentence name sort =
    ObjectSentence
        ( SentenceSymbolSentence SentenceSymbol
            { sentenceSymbolSymbol     = Symbol
                { symbolConstructor = Id name
                , symbolParams      = []
                }
            , sentenceSymbolSorts      = []
            , sentenceSymbolResultSort = sort
            , sentenceSymbolAttributes = Attributes []
            }
        )

associativeObjectSymbolSentence :: KoreSentence
associativeObjectSymbolSentence =
    noArgumentOrParameterSentence "associative" attributeObjectSort

commutativeObjectSymbolSentence :: KoreSentence
commutativeObjectSymbolSentence =
    noArgumentOrParameterSentence "commutative" attributeObjectSort

uncheckedAttributesModule :: KoreModule
uncheckedAttributesModule =
    Module
        { moduleName       = ModuleName "BUILTIN-attributes"
        , moduleSentences  =
            [ attributeObjectSortSentence
            , hookObjectSortSentence
            , hookObjectSymbolSentence
            , argumentPositionObjectSortSentence
            , firstArgumentObjectSymbolSentence
            , secondArgumentObjectSymbolSentence
            , thirdArgumentObjectSymbolSentence
            , fourthArgumentObjectSymbolSentence
            , fifthArgumentObjectSymbolSentence
            , strictObjectSortSentence
            , strictObjectSymbolSentence
            , associativeObjectSymbolSentence
            , commutativeObjectSymbolSentence
            ]
        , moduleAttributes = Attributes []
        }
