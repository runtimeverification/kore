module Test.Kore.Unparser
    ( test_parse
    , test_unparse
    ) where

import           Hedgehog
                 ( Gen, Property, (===) )
import qualified Hedgehog
import           Test.Tasty
                 ( TestTree, testGroup )
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit
                 ( assertEqual, testCase )

import Kore.AST.Pure
import Kore.AST.Sentence
import Kore.Parser.Lexeme
import Kore.Parser.Parser
import Kore.Parser.ParserUtils
import Kore.Unparser

import Test.Kore

test_unparse :: TestTree
test_unparse =
    testGroup
        "Unparse"
        [ unparseTest
            (asSentence
                (SentenceSort
                    { sentenceSortName = testId "x"
                    , sentenceSortParameters = []
                    , sentenceSortAttributes = Attributes []
                    }
                    :: ParsedSentenceSort
                )
            )
            "sort x{} []"
        , unparseTest
            Attributes
                { getAttributes =
                    [ asParsedPattern (TopPattern Top
                        { topSort = SortVariableSort SortVariable
                            { getSortVariable = testId "#Fm" }
                        })
                    , asParsedPattern (InPattern In
                        { inOperandSort = SortActualSort SortActual
                            { sortActualName = testId "B"
                            , sortActualSorts = []
                            }
                        , inResultSort = SortActualSort SortActual
                            { sortActualName = testId "G"
                            , sortActualSorts = []
                            }
                        , inContainedChild =
                            asParsedPattern $ VariablePattern Variable
                                { variableName = testId "T"
                                , variableSort = SortVariableSort SortVariable
                                    { getSortVariable = testId "C" }
                                , variableCounter = mempty
                                }
                        , inContainingChild = asParsedPattern (StringLiteralPattern
                            StringLiteral { getStringLiteral = "" })
                        })
                    ]
                }
            "[\\top{#Fm}(), \\in{B{}, G{}}(T:C, \"\")]"
        , unparseTest
            (Module
                { moduleName = ModuleName "t"
                , moduleSentences = []
                , moduleAttributes = Attributes []
                }
                :: ParsedModule
            )
            "module t\n\
            \endmodule\n\
            \[]"
        , unparseParseTest
            koreDefinitionParser
            Definition
                { definitionAttributes = Attributes {getAttributes = []}
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName {getModuleName = "i"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    , Module
                        { moduleName = ModuleName {getModuleName = "k"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    ]
                }
        , unparseTest
            (Definition
                { definitionAttributes = Attributes {getAttributes = []}
                , definitionModules =
                    [ Module
                        { moduleName = ModuleName {getModuleName = "i"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    , Module
                        { moduleName = ModuleName {getModuleName = "k"}
                        , moduleSentences = []
                        , moduleAttributes = Attributes {getAttributes = []}
                        }
                    ]
                }
                :: ParsedDefinition
            )
            "[]\n\
            \module i\n\
            \endmodule\n\
            \[]\n\
            \module k\n\
            \endmodule\n\
            \[]"
        , unparseTest
            ( SentenceImportSentence SentenceImport
                { sentenceImportModuleName = ModuleName {getModuleName = "sl"}
                , sentenceImportAttributes =
                    Attributes { getAttributes = [] } :: Attributes
                }
                :: ParsedSentence
            )
            "import sl []"
        , unparseTest
            (Attributes
                { getAttributes =
                    [ asParsedPattern
                        ( TopPattern Top
                            { topSort = SortActualSort SortActual
                                { sortActualName = testId "#CharList"
                                , sortActualSorts = []
                                }
                            }
                        )
                    ]
                }::Attributes
            )
            "[\\top{#CharList{}}()]"
        , unparseTest
            (Attributes
                { getAttributes =
                    [ asParsedPattern
                        (CharLiteralPattern CharLiteral
                            { getCharLiteral = '\'' }
                        )
                    , asParsedPattern
                        (CharLiteralPattern CharLiteral
                            { getCharLiteral = '\'' }
                        )
                    ]
                }::Attributes
            )
            "[''', ''']"
        ]

test_parse :: TestTree
test_parse =
    testGroup
        "Parse"
        [ testProperty "Object testId" $
            roundtrip (idGen IsObject) (idParser Object)
        , testProperty "StringLiteral" $
            roundtrip stringLiteralGen stringLiteralParser
        , testProperty "CharLiteral" $
            roundtrip charLiteralGen charLiteralParser
        , testProperty "Object Symbol" $
            roundtrip symbolGen (symbolParser Object)
        , testProperty "Meta Symbol" $
            roundtrip symbolGen (symbolParser Meta)
        , testProperty "Object Alias" $
            roundtrip aliasGen (aliasParser Object)
        , testProperty "Meta Alias" $
            roundtrip aliasGen (aliasParser Meta)
        , testProperty "Object SortVariable" $
            roundtrip sortVariableGen (sortVariableParser Object)
        , testProperty "Meta SortVariable" $
            roundtrip sortVariableGen (sortVariableParser Meta)
        , testProperty "Object Sort" $
            roundtrip (standaloneGen sortGen) (sortParser Object)
        , testProperty "Meta Sort" $
            roundtrip (standaloneGen sortGen) (sortParser Meta)
        , testProperty "UnifiedVariable" $
            roundtrip
                (standaloneGen $ variableGen =<< sortGen)
                (variableParser Object)
        , testProperty "ParsedPattern" $
            roundtrip korePatternGen korePatternParser
        , testProperty "Attributes" $
            roundtrip (standaloneGen attributesGen) attributesParser
        , testProperty "Sentence" $
            roundtrip (standaloneGen koreSentenceGen) koreSentenceParser
        , testProperty "Module" $
            roundtrip
                (standaloneGen $ moduleGen koreSentenceGen)
                (moduleParser koreSentenceParser)
        , testProperty "Definition" $
            roundtrip
                (standaloneGen $ definitionGen koreSentenceGen)
                (definitionParser koreSentenceParser)
        ]

parse :: Parser a -> String -> Either String a
parse parser =
    parseOnly (parser <* endOfInput) "<test-string>"

roundtrip :: (Unparse a, Eq a, Show a) => Gen a -> Parser a -> Property
roundtrip generator parser =
    Hedgehog.property $ do
        generated <- Hedgehog.forAll generator
        parse parser (unparseToString generated) === Right generated

unparseParseTest
    :: (Unparse a, Eq a, Show a) => Parser a -> a -> TestTree
unparseParseTest parser astInput =
    testCase
        "Parsing + unparsing."
        (assertEqual ""
            (Right astInput)
            (parse parser (unparseToString astInput)))

unparseTest :: (Unparse a, Show a) => a -> String -> TestTree
unparseTest astInput expected =
    testCase
        "Unparsing"
        (assertEqual (show astInput)
            expected
            (unparseToString astInput))
