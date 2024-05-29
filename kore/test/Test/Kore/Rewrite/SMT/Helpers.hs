module Test.Kore.Rewrite.SMT.Helpers (
    atom,
    list,
    eq,
    gt,
    lt,
    ofType,
    isError,
    isNotSatisfiable,
    isSatisfiable,
    isSatisfiableWithTools,
    isNotSatisfiableWithTools,
    constructorAxiom,
    testsForModule,
) where

import Control.Exception (
    ErrorCall,
    catch,
 )
import Control.Lens qualified as Lens
import Data.Generics.Product (
    field,
 )
import Data.Limit
import Data.Sup (
    Sup (Element),
 )
import Data.Text (
    Text,
 )
import Kore.Attribute.Attributes
import Kore.Attribute.Symbol qualified as Attribute (
    Symbol,
 )
import Kore.IndexedModule.IndexedModule (
    VerifiedModule,
 )
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools (
    build,
 )
import Kore.Internal.Symbol
import Kore.Internal.TermLike
import Kore.Syntax.Sentence (
    ParsedSentence,
    Sentence (..),
    SentenceAxiom (SentenceAxiom),
 )
import Kore.Syntax.Sentence qualified as SentenceAxiom (
    SentenceAxiom (..),
 )
import Numeric.Natural (
    Natural,
 )
import Prelude.Kore
import SMT (
    Config (..),
    SMT,
    TimeOut (..),
    z3Config,
 )
import SMT qualified
import Test.Kore (
    testId,
 )
import Test.Kore.Builtin.Builtin (
    runSMTWithConfig,
 )
import Test.Kore.Builtin.External
import Test.Kore.IndexedModule.MockMetadataTools qualified as Mock
import Test.Kore.Rewrite.SMT.Builders (
    noJunk,
 )
import Test.Kore.With (
    with,
 )
import Test.Tasty
import Test.Tasty.HUnit

newtype SmtPrelude = SmtPrelude {getSmtPrelude :: SMT ()}

ofType :: SMT.MonadSMT m => Text -> Text -> m ()
name `ofType` constType = do
    _ <- SMT.declare name "" (atom constType)
    return ()

atom :: Text -> SMT.SExpr
atom = SMT.Atom

list :: [SMT.SExpr] -> SMT.SExpr
list = SMT.List

gt :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
gt = SMT.gt

lt :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
lt = SMT.lt

eq :: SMT.SExpr -> SMT.SExpr -> SMT.SExpr
eq = SMT.eq

isSatisfiable ::
    HasCallStack =>
    [SMT ()] ->
    SmtMetadataTools Attribute.Symbol ->
    SmtPrelude ->
    TestTree
isSatisfiable tests _ = assertSmtTestCase "isSatisfiable" (Just SMT.Sat) tests

isSatisfiableWithTools ::
    HasCallStack =>
    [SmtMetadataTools Attribute.Symbol -> SMT ()] ->
    SmtMetadataTools Attribute.Symbol ->
    SmtPrelude ->
    TestTree
isSatisfiableWithTools tests tools prelude =
    assertSmtTestCase
        "isSatisfiable"
        (Just SMT.Sat)
        (fmap (\t -> t tools) tests)
        prelude

isNotSatisfiable ::
    HasCallStack =>
    [SMT ()] ->
    SmtMetadataTools Attribute.Symbol ->
    SmtPrelude ->
    TestTree
isNotSatisfiable tests _ = assertSmtTestCase "isNotSatisfiable" (Just SMT.Unsat) tests

isNotSatisfiableWithTools ::
    HasCallStack =>
    [SmtMetadataTools Attribute.Symbol -> SMT ()] ->
    SmtMetadataTools Attribute.Symbol ->
    SmtPrelude ->
    TestTree
isNotSatisfiableWithTools tests tools prelude =
    assertSmtTestCase
        "isNotSatisfiable"
        (Just SMT.Unsat)
        (fmap (\t -> t tools) tests)
        prelude

isError ::
    HasCallStack =>
    [SMT ()] ->
    SmtMetadataTools Attribute.Symbol ->
    SmtPrelude ->
    TestTree
isError actions _ prelude =
    testCase "isError" $
        catch (catch runSolver ignoreIOError) ignoreErrorCall
  where
    runSolver = do
        _ <- getSmtResult actions prelude
        assertFailure "No `error` was raised."

    ignoreIOError :: IOError -> IO ()
    ignoreIOError _err =
        return ()

    ignoreErrorCall :: ErrorCall -> IO ()
    ignoreErrorCall _err =
        return ()

getSmtResult ::
    [SMT ()] ->
    SmtPrelude ->
    IO (Maybe SMT.Result)
getSmtResult
    actions
    SmtPrelude{getSmtPrelude = preludeAction} =
        do
            let smtResult :: SMT (Maybe SMT.Result)
                smtResult = do
                    sequence_ actions
                    SMT.check
            runSMTWithConfig
                z3Config{timeOut = TimeOut (Limit 100)}
                preludeAction
                smtResult

assertSmtResult ::
    HasCallStack =>
    Maybe (SMT.Result) ->
    [SMT ()] ->
    SmtPrelude ->
    Assertion
assertSmtResult expected actions prelude = do
    result <- getSmtResult actions prelude
    assertEqual "" expected result

assertSmtTestCase ::
    HasCallStack =>
    String ->
    Maybe SMT.Result ->
    [SMT ()] ->
    SmtPrelude ->
    TestTree
assertSmtTestCase name expected actions prelude =
    testCase name $ assertSmtResult expected actions prelude

testsForModule ::
    String ->
    ( SmtMetadataTools Attribute.Symbol ->
      VerifiedModule Attribute.Symbol ->
      SMT ()
    ) ->
    VerifiedModule Attribute.Symbol ->
    [SmtMetadataTools Attribute.Symbol -> SmtPrelude -> TestTree] ->
    TestTree
testsForModule name functionToTest indexedModule tests =
    testGroup name (map (\f -> f tools prelude) tests)
  where
    prelude =
        SmtPrelude
            (functionToTest tools indexedModule)
    tools = MetadataTools.build indexedModule

constructorAxiom :: Text -> [(Text, [Text])] -> ParsedSentence
constructorAxiom sortName constructors =
    SentenceAxiomSentence
        SentenceAxiom
            { sentenceAxiomParameters = []
            , sentenceAxiomPattern =
                externalize $
                    foldr mkOr (mkBottom sort) constructorAssertions
            , sentenceAxiomAttributes = Attributes []
            }
        `with` noJunk
  where
    sort = makeSort sortName
    constructorAssertions =
        map constructorAssertion constructors
    constructorAssertion (constructorName, argumentSorts) =
        foldr
            mkExists
            (mkApplySymbol symbol (map mkElemVar argumentVariables))
            argumentVariables
      where
        argumentVariables :: [ElementVariable VariableName]
        argumentVariables = zipWith makeVariable [1 ..] argumentSorts

        symbol =
            Symbol
                { symbolConstructor = testId constructorName
                , symbolParams = []
                , symbolAttributes = Mock.constructorTotalAttributes
                , symbolSorts =
                    applicationSorts (map makeSort argumentSorts) sort
                }

makeVariable :: Natural -> Text -> ElementVariable VariableName
makeVariable varIndex sortName =
    mkElementVariable (testId "var") (makeSort sortName)
        & Lens.set
            (field @"variableName" . Lens.mapped . field @"counter")
            (Just (Element varIndex))

makeSort :: Text -> Sort
makeSort name =
    SortActualSort
        SortActual
            { sortActualName = testId name
            , sortActualSorts = []
            }
