module Test.Kore.ASTVerifier.DefinitionVerifier.SortUsage
    ( test_sortUsage
    ) where

import Test.Tasty
       ( TestTree, testGroup )
import Test.Tasty.HUnit
       ( HasCallStack )

import qualified Data.List as List
import           Data.Maybe
                 ( mapMaybe )
import qualified Data.Text as Text

import           Kore.AST.Pure
import           Kore.AST.Sentence
import           Kore.AST.Valid
                 ( mkTop )
import           Kore.Error
import           Kore.IndexedModule.Error
                 ( noSort )
import qualified Kore.Verified as Verified

import Test.Kore
import Test.Kore.ASTVerifier.DefinitionVerifier

data TestFlag
    = CannotSeeSortVariables
    | CannotSeeSortDeclarations
    deriving Eq

data AdditionalTestConfiguration
    = SkipTest
    | AdditionalSentences [Verified.Sentence]

data TestConfiguration = TestConfiguration
    { testConfigurationDescription :: !String
    , testConfigurationAdditionalSentences :: ![Verified.Sentence]
    , testConfigurationAdditionalSortVariables :: ![SortVariable]
    , testConfigurationCaseBasedConfiguration
        :: ![([TestFlag], AdditionalTestConfiguration)]
    }

data SuccessConfiguration
    = SuccessConfiguration TestConfiguration
    | SuccessConfigurationSkipAll
data FailureConfiguration
    = FailureConfiguration TestConfiguration
    | FailureConfigurationSkipAll

data FlaggedTestData = FlaggedTestData
    { flaggedTestDataFlags    :: ![TestFlag]
    , flaggedTestDataTestData :: !([Verified.Sentence] -> TestData)
    }

test_sortUsage :: [TestTree]
test_sortUsage =
    [ expectSuccess "Simplest definition"
        (simpleDefinitionFromSentences (ModuleName "MODULE") [])
    , expectSuccess "Definition with sort"
        ( simpleDefinitionFromSentences (ModuleName "MODULE")
            [ sortSentenceWithSortParameters
                (SortName "sFailureDescription") []
            ]
        )
    , expectSuccess "Definition with meta alias"
        ( simpleDefinitionFromSentences (ModuleName "MODULE")
            [ metaAliasSentenceWithSortParameters
                (AliasName "#a") stringMetaSort []
            ]
        )
    , testsForObjectSort
        (CommonDescription "Referencing simple sort")
        (SuccessConfiguration TestConfiguration
            { testConfigurationDescription = "The sort is declared"
            , testConfigurationAdditionalSentences =
                [ simpleSortSentence (SortName "s") ]
            , testConfigurationAdditionalSortVariables = []
            , testConfigurationCaseBasedConfiguration =
                [ ([CannotSeeSortDeclarations], SkipTest) ]
            }
        )
        (FailureConfiguration TestConfiguration
            { testConfigurationDescription = "The sort is not declared"
            , testConfigurationAdditionalSentences = []
            , testConfigurationAdditionalSortVariables = []
            , testConfigurationCaseBasedConfiguration =
                [
                    ( [ CannotSeeSortDeclarations ]
                    , AdditionalSentences
                        [ simpleSortSentence (SortName "s") ]
                    )
                ]
            }
        )
        (ExpectedErrorMessage $ noSort "s")
        (ErrorStack ["sort 's' (<test data>)", "(<test data>)"])
        (TestedSort (simpleSort (SortName "s")))
        (NamePrefix "#internal")
    , testsForObjectSort
        (CommonDescription "Referencing sort variable")
        (SuccessConfiguration TestConfiguration
            { testConfigurationDescription = "The variable is declared"
            , testConfigurationAdditionalSentences = []
            , testConfigurationAdditionalSortVariables = [sortVariable "s"]
            , testConfigurationCaseBasedConfiguration =
                [
                    ( [CannotSeeSortDeclarations, CannotSeeSortVariables]
                    , SkipTest
                    )
                ]
            }
        )
        (FailureConfiguration TestConfiguration
            { testConfigurationDescription = "The variable is not declared"
            , testConfigurationAdditionalSentences = []
            , testConfigurationAdditionalSortVariables = []
            , testConfigurationCaseBasedConfiguration = []
            }
        )
        (ExpectedErrorMessage "Sort variable 's' not declared.")
        (ErrorStack ["(<test data>)"])
        (TestedSort (objectVariableSort "s"))
        (NamePrefix "internal")
    , let
        referencingSortVariableTestConfiguration = TestConfiguration
            { testConfigurationDescription = "Referencing sort variable"
            , testConfigurationAdditionalSentences =
                [ simpleSortSentence additionalSortName ]
            , testConfigurationAdditionalSortVariables =
                [ sortVariable "s" ]
            , testConfigurationCaseBasedConfiguration =
                [
                    ( [CannotSeeSortDeclarations, CannotSeeSortVariables]
                    , SkipTest
                    )
                ]
            }
      in
        expectSuccessFlaggedTests
            (SuccessConfiguration referencingSortVariableTestConfiguration)
            (flaggedObjectTestsForSort
                referencingSortVariableTestConfiguration
                (TestedSort (objectVariableSort "s"))
                (SortActualThatIsDeclared
                    (simpleSortActual additionalSortName))
                (NamePrefix "internal")
            )
    , failureTestsForMetaSort
        (CommonDescription "Referencing simple sort")
        (FailureConfiguration TestConfiguration
            { testConfigurationDescription = "The sort is not declared"
            , testConfigurationAdditionalSentences = []
            , testConfigurationAdditionalSortVariables = []
            , testConfigurationCaseBasedConfiguration = []
            }
        )
        (ExpectedErrorMessage $ noSort "#s")
        (ErrorStack ["sort '#s' (<test data>)", "(<test data>)"])
        (TestedSort (simpleSort (SortName "#s")))
        (SortActualThatIsDeclared (simpleSortActual (SortName "#Char")))
        (NamePrefix "#internal")
    , successTestsForMetaSort
        (CommonDescription "Referencing simple sort")
        (SuccessConfiguration TestConfiguration
            { testConfigurationDescription = "The sort is declared"
            , testConfigurationAdditionalSentences = []
            , testConfigurationAdditionalSortVariables =
                [ sortVariable "#s" ]
            , testConfigurationCaseBasedConfiguration =
                [([CannotSeeSortVariables], SkipTest)]
            }
        )
        (TestedSort (sortVariableSort "#s"))
        (SortActualThatIsDeclared (simpleSortActual (SortName "#Char")))
        (NamePrefix "#internal")
    , testsForObjectSort
        (CommonDescription "Referencing parametrized sort")
        (SuccessConfiguration TestConfiguration
            { testConfigurationDescription = "Correct sort count"
            , testConfigurationAdditionalSentences =
                [ simpleSortSentence additionalSortName
                , asSentence
                    (SentenceSort
                        { sentenceSortName = testId "UnarySort"
                        , sentenceSortParameters = [ SortVariable (testId "svn") ]
                        , sentenceSortAttributes =
                            Attributes []
                        }
                    :: Verified.SentenceSort)
                ]
            , testConfigurationAdditionalSortVariables = []
            , testConfigurationCaseBasedConfiguration =
                [ ([CannotSeeSortDeclarations], SkipTest) ]
            }
        )
        FailureConfigurationSkipAll
        (ExpectedErrorMessage "None")
        (ErrorStack [])
        (TestedSort
            (SortActualSort SortActual
                { sortActualName = testId "UnarySort"
                , sortActualSorts = [ simpleSort additionalSortName ]
                }
            )
        )
        (NamePrefix "internal")
    , testsForObjectSort
        (CommonDescription "Referencing parametrized sort")
        SuccessConfigurationSkipAll
        (FailureConfiguration TestConfiguration
            { testConfigurationDescription = "Wrong sort count"
            , testConfigurationAdditionalSentences =
                [ simpleSortSentence additionalSortName
                , asSentence
                    (SentenceSort
                        { sentenceSortName = testId "UnarySort"
                        , sentenceSortParameters = [ SortVariable (testId "svn") ]
                        , sentenceSortAttributes =
                            Attributes []
                        }
                    :: Verified.SentenceSort)
                ]
            , testConfigurationAdditionalSortVariables = []
            , testConfigurationCaseBasedConfiguration =
                [ ([CannotSeeSortDeclarations], SkipTest) ]
            }
        )
        (ExpectedErrorMessage "Expected 1 sort arguments, but got 2.")
        (ErrorStack ["sort 'UnarySort' (<test data>)"])
        (TestedSort
            (SortActualSort SortActual
                { sortActualName = testId "UnarySort"
                , sortActualSorts =
                    [ simpleSort additionalSortName
                    , simpleSort additionalSortName]
                }
            )
        )
        (NamePrefix "internal")
    ]
  where
    additionalSortName = SortName "additionalSort1"

newtype CommonDescription = CommonDescription String

testsForObjectSort
    :: HasCallStack
    => CommonDescription
    -> SuccessConfiguration
    -> FailureConfiguration
    -> ExpectedErrorMessage
    -> ErrorStack
    -> TestedSort Object
    -> NamePrefix
    -> TestTree
testsForObjectSort
    (CommonDescription commonDescription)
    successConfiguration
    failureConfiguration
    expectedErrorMessage
    errorStack
    sort
    namePrefix@(NamePrefix rawNamePrefix)
  =
    testGroup
        commonDescription
        (
            (case successConfiguration of
                SuccessConfigurationSkipAll -> []
                SuccessConfiguration testConfiguration ->
                    let
                        successTestConfiguration =
                            addAdditionalSortSentence testConfiguration
                    in
                        [ expectSuccessFlaggedTests
                            (SuccessConfiguration successTestConfiguration)
                            (testConfigurationToFlaggedTests
                                successTestConfiguration)
                        ]
            )
            ++
            (case failureConfiguration of
                FailureConfigurationSkipAll -> []
                FailureConfiguration testConfiguration ->
                    let
                        failureTestConfiguration =
                            addAdditionalSortSentence testConfiguration
                    in
                        [ expectFailureWithErrorFlaggedTests
                            (FailureConfiguration failureTestConfiguration)
                            expectedErrorMessage
                            errorStack
                            (testConfigurationToFlaggedTests
                                failureTestConfiguration)
                        ]
            )
        )
  where
    testConfigurationToFlaggedTests configuration =
        flaggedObjectTestsForSort
            configuration
            sort
            (SortActualThatIsDeclared additionalSortActual)
            namePrefix
    additionalSortActualName = SortName (rawNamePrefix <> "_declaredSort")
    additionalSortActual = simpleSortActual additionalSortActualName
    additionalSortSentence = simpleSortSentence additionalSortActualName
    addAdditionalSortSentence =
        addSentenceToTestConfiguration additionalSortSentence

addSentenceToTestConfiguration
    :: Verified.Sentence
    -> TestConfiguration
    -> TestConfiguration
addSentenceToTestConfiguration
    sentence
    configuration@TestConfiguration
        { testConfigurationAdditionalSentences = existingSentences }
  =
    configuration
        { testConfigurationAdditionalSentences = sentence : existingSentences }

successTestsForMetaSort
    :: CommonDescription
    -> SuccessConfiguration
    -> TestedSort Meta
    -> SortActualThatIsDeclared Meta
    -> NamePrefix
    -> TestTree
successTestsForMetaSort
    (CommonDescription commonDescription)
    successConfiguration@(SuccessConfiguration testConfiguration)
    sort
    additionalSortActual
    namePrefix
  =
    testGroup
        commonDescription
        [ expectSuccessFlaggedTests successConfiguration flaggedTests]
  where
    flaggedTests =
        flaggedMetaTestsForSort
            testConfiguration
            sort
            additionalSortActual
            namePrefix
successTestsForMetaSort
    (CommonDescription commonDescription) SuccessConfigurationSkipAll _ _ _
  = testGroup commonDescription []

failureTestsForMetaSort
    :: CommonDescription
    -> FailureConfiguration
    -> ExpectedErrorMessage
    -> ErrorStack
    -> TestedSort Meta
    -> SortActualThatIsDeclared Meta
    -> NamePrefix
    -> TestTree
failureTestsForMetaSort
    (CommonDescription commonDescription)
    failureConfiguration@(FailureConfiguration testConfiguration)
    expectedErrorMessage
    errorStack
    sort
    additionalSortActual
    namePrefix
  =
    testGroup
        commonDescription
        [ expectFailureWithErrorFlaggedTests
            failureConfiguration expectedErrorMessage errorStack flaggedTests
        ]
  where
    flaggedTests =
        flaggedMetaTestsForSort
            testConfiguration
            sort
            additionalSortActual
            namePrefix
failureTestsForMetaSort
    (CommonDescription commonDescription) FailureConfigurationSkipAll _ _ _ _ _
  = testGroup commonDescription []

expectSuccessFlaggedTests
    :: SuccessConfiguration
    -> [FlaggedTestData]
    -> TestTree
expectSuccessFlaggedTests
    (SuccessConfiguration testConfiguration)
    flaggedTests
  =
    testGroup (testConfigurationDescription testConfiguration)
        (map successTestData
            (applyTestConfiguration testConfiguration flaggedTests)
        )
expectSuccessFlaggedTests SuccessConfigurationSkipAll _ = testGroup "" []

expectFailureWithErrorFlaggedTests
    :: HasCallStack
    => FailureConfiguration
    -> ExpectedErrorMessage
    -> ErrorStack
    -> [FlaggedTestData]
    -> TestTree
expectFailureWithErrorFlaggedTests
    (FailureConfiguration testConfiguration)
    errorMessage
    additionalErrorStack
    flaggedTests
  =
    testGroup (testConfigurationDescription testConfiguration)
        (map
            (failureTestData errorMessage additionalErrorStack)
            (applyTestConfiguration testConfiguration flaggedTests)
        )
expectFailureWithErrorFlaggedTests FailureConfigurationSkipAll _ _ _ =
    testGroup "" []

flaggedObjectTestsForSort
    :: TestConfiguration
    -> TestedSort Object
    -> SortActualThatIsDeclared Object
    -> NamePrefix
    -> [FlaggedTestData]
flaggedObjectTestsForSort
    testConfiguration
    sort
    additionalSortActual
    namePrefix
  =
    unfilteredTestExamplesForSort
        sort
        additionalSortActual
        sortVariables
        namePrefix
        asSentence
        asSentence
    ++ unfilteredTestExamplesForObjectSort
        sort
        additionalSortActual
        sortVariables
        namePrefix
  where
    sortVariables =
        testConfigurationAdditionalSortVariables testConfiguration

flaggedMetaTestsForSort
    :: TestConfiguration
    -> TestedSort Meta
    -> SortActualThatIsDeclared Meta
    -> NamePrefix
    -> [FlaggedTestData]
flaggedMetaTestsForSort
    testConfiguration
    sort
    additionalSortActual
    namePrefix
  =
    unfilteredTestExamplesForSort
        sort
        additionalSortActual
        (testConfigurationAdditionalSortVariables testConfiguration)
        namePrefix
        asSentence
        asSentence

applyTestConfiguration
    :: TestConfiguration
    -> [FlaggedTestData]
    -> [TestData]
applyTestConfiguration testConfiguration =
    mapMaybe (applyOneTestConfiguration testConfiguration)

applyOneTestConfiguration
    :: TestConfiguration
    -> FlaggedTestData
    -> Maybe TestData
applyOneTestConfiguration testConfiguration flaggedTestData =
  case currentConfiguration of
    Nothing -> Just (testDataFunction additionalSentences)
    Just SkipTest -> Nothing
    Just (AdditionalSentences moreSentences) ->
        Just (testDataFunction (additionalSentences ++ moreSentences))
  where
    additionalSentences = testConfigurationAdditionalSentences testConfiguration
    testDataFunction = flaggedTestDataTestData flaggedTestData
    currentConfiguration =
        snd <$>
            List.find
                testHasFlags
                (testConfigurationCaseBasedConfiguration testConfiguration)
    testHasFlags configurationWithFlags =
        any
            (`elem` flaggedTestDataFlags flaggedTestData)
            (fst configurationWithFlags)

newtype TestedSort level = TestedSort Sort
newtype SortActualThatIsDeclared level =
    SortActualThatIsDeclared SortActual

unfilteredTestExamplesForSort
    :: TestedSort level
    -> SortActualThatIsDeclared level
    -> [SortVariable]
    -> NamePrefix
    -> (Verified.SentenceAlias -> Verified.Sentence)
    -> (Verified.SentenceSymbol -> Verified.Sentence)
    -> [FlaggedTestData]
unfilteredTestExamplesForSort
    (TestedSort sort)
    (SortActualThatIsDeclared additionalSortActual)
    sortVariables
    (NamePrefix identifierPrefix)
    sentenceAliasSentence
    sentenceSymbolSentence
  =
    [ FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription = "Alias definition with result sort"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "alias '"
                        ++ Text.unpack rawAliasName
                        ++ "' declaration (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    (sentenceAliasSentence
                        (sentenceAliasWithResultSort
                            aliasName
                            sort
                            sortVariables
                            (mkTop sort)
                        )
                    : additionalSentences
                    )
            }
        }
    , FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription = "Alias definition with sort argument"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "alias '"
                        ++ Text.unpack rawAliasName
                        ++ "' declaration (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    (sentenceAliasSentence
                        (sentenceAliasWithSortArgument
                            aliasName
                            sort
                            additionalSort
                            sortVariables
                            (mkTop additionalSort)
                        )
                    : additionalSentences
                    )
            }
        }
    , FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription =
                "Definition with axiom and binder of sort"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "axiom declaration"
                    , "\\exists '"
                        ++ Text.unpack rawVariableName
                        ++ "' (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    ( axiomSentenceWithSortParameters
                        (simpleExistsUnifiedPattern variableName1 sort)
                        sortVariables
                    : additionalSentences
                    )
            }
        }
    , FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription =
                "Definition with ML pattern and operand sort"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "axiom declaration"
                    , "\\exists '"
                        ++ Text.unpack rawVariableName
                        ++ "' (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    ( axiomSentenceWithSortParameters
                        ( simpleExistsEqualsUnifiedPattern
                            variableName1
                            (OperandSort sort)
                            (ResultSort additionalSort)
                        )
                        sortVariables
                    : additionalSentences
                    )
            }
        }
    , FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription =
                "Definition with ML pattern and operand sort"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "axiom declaration"
                    , "\\exists '"
                        ++ Text.unpack rawVariableName
                        ++ "' (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    ( axiomSentenceWithSortParameters
                        ( simpleExistsEqualsUnifiedPattern
                            variableName1
                            (OperandSort additionalSort)
                            (ResultSort sort)
                        )
                        sortVariables
                    : additionalSentences
                    )
            }
        }
    , FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription = "Definition with application pattern"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "axiom declaration"
                    , "symbol or alias '"
                        ++ Text.unpack rawAliasName
                        ++ "' (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    ( axiomSentenceWithSortParameters
                        ( applicationUnifiedPatternWithParams
                            additionalSort
                            (SymbolName rawAliasName)
                            [sort]
                        )
                        sortVariables
                    : sentenceSymbolSentence
                        (symbolSentenceWithSortParameters
                            (SymbolName rawAliasName)
                            additionalSortName
                            [sortVariable sortVariableName1]
                        )
                    : additionalSentences
                    )
            }
        }
    ]
  where
    rawAliasName = identifierPrefix <> "_alias"
    aliasName = AliasName rawAliasName
    rawVariableName = identifierPrefix <> "_variable"
    variableName1 = VariableName rawVariableName
    sortVariableName1 = identifierPrefix <> "_sortVariable"
    additionalSortRawName = getId (sortActualName additionalSortActual)
    additionalSortName = SortName additionalSortRawName
    additionalSort = SortActualSort additionalSortActual
    defaultErrorMessage = "Replace this with a real error message."

unfilteredTestExamplesForObjectSort
    :: TestedSort Object
    -> SortActualThatIsDeclared Object
    -> [SortVariable]
    -> NamePrefix
    -> [FlaggedTestData]
unfilteredTestExamplesForObjectSort
    (TestedSort sort)
    (SortActualThatIsDeclared additionalSortActual)
    sortVariables
    (NamePrefix namePrefix)
  =
    [ FlaggedTestData
        { flaggedTestDataFlags = []
        , flaggedTestDataTestData = \additionalSentences -> TestData
            { testDataDescription = "Definition with complex sort"
            , testDataError =
                Error
                    [ "module 'MODULE'"
                    , "axiom declaration"
                    , "symbol or alias 'a' (<test data>)"
                    , "sort '"
                        ++ Text.unpack differentAdditionalSortRawName
                        ++ "' (<test data>)"
                    ]
                    defaultErrorMessage
            , testDataDefinition =
                simpleDefinitionFromSentences
                    (ModuleName "MODULE")
                    ( axiomSentenceWithSortParameters
                        (applicationUnifiedPatternWithParams
                            resultSort
                            (SymbolName "a")
                            [ SortActualSort SortActual
                                { sortActualName =
                                    testId differentAdditionalSortRawName
                                , sortActualSorts = [sort]
                                }
                            ]
                        )
                        sortVariables
                    : symbolSentenceWithResultSort
                        (SymbolName "a")
                        resultSort
                        [sortVariable sortVariableName1]
                    : sortSentenceWithSortParameters
                        differentAdditionalSortName
                        [sortVariable sortVariableName2]
                    : additionalSentences
                    )
            }
        }
    ]
  where
    sortVariableName1 = namePrefix <> "_sortVariable1"
    sortVariableName2 = namePrefix <> "_sortVariable2"
    additionalSortRawName = getId (sortActualName additionalSortActual)
    differentAdditionalSortRawName = additionalSortRawName <> "1"
    differentAdditionalSortName = SortName differentAdditionalSortRawName
    defaultErrorMessage = "Replace this with a real error message."
    resultSort =
        SortActualSort SortActual
            { sortActualName = testId differentAdditionalSortRawName
            , sortActualSorts =
                [ objectVariableSort sortVariableName1 ]
            }
