{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, errorHandler, config, ...
}:
{
  flags = { threaded = true; };
  package = {
    specVersion = "2.2";
    identifier = {
      name = "kore";
      version = "0.60.0.0";
    };
    license = "BSD-3-Clause";
    copyright = "2018-2021 Runtime Verification Inc";
    maintainer = "ana.pantilie@runtimeverification.com";
    author = "Runtime Verification Inc";
    homepage = "https://github.com/runtimeverification/haskell-backend#readme";
    url = "";
    synopsis = "";
    description = "Please see the [README](README.md) file.";
    buildType = "Simple";
    isLocal = true;
    detailLevel = "FullDetails";
    licenseFiles = [ "LICENSE" ];
    dataDir = ".";
    dataFiles = [ ];
    extraSrcFiles = [ "README.md" "CHANGELOG.md" ];
    extraTmpFiles = [ ];
    extraDocFiles = [ ];
  };
  components = {
    "library" = {
      depends = [
        (hsPkgs."base" or (errorHandler.buildDepError "base"))
        (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
        (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
        (hsPkgs."timeit" or (errorHandler.buildDepError "timeit"))
        (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
        (hsPkgs."array" or (errorHandler.buildDepError "array"))
        (hsPkgs."async" or (errorHandler.buildDepError "async"))
        (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
        (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
        (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
        (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
        (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
        (hsPkgs."compact" or (errorHandler.buildDepError "compact"))
        (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
        (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
        (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
        (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
        (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
        (hsPkgs."decision-diagrams" or (errorHandler.buildDepError
          "decision-diagrams"))
        (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
        (hsPkgs."deriving-aeson" or (errorHandler.buildDepError
          "deriving-aeson"))
        (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
        (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
        (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
        (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
        (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
        (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
        (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
        (hsPkgs."free" or (errorHandler.buildDepError "free"))
        (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
        (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
        (hsPkgs."ghc-events" or (errorHandler.buildDepError "ghc-events"))
        (hsPkgs."ghc-trace-events" or (errorHandler.buildDepError
          "ghc-trace-events"))
        (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
        (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
        (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
        (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
        (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
        (hsPkgs."json-rpc" or (errorHandler.buildDepError "json-rpc"))
        (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
        (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
        (hsPkgs."matrix" or (errorHandler.buildDepError "matrix"))
        (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
        (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
        (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
        (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
        (hsPkgs."mono-traversable" or (errorHandler.buildDepError
          "mono-traversable"))
        (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
        (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
        (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
          "optparse-applicative"))
        (hsPkgs."parser-combinators" or (errorHandler.buildDepError
          "parser-combinators"))
        (hsPkgs."pqueue" or (errorHandler.buildDepError "pqueue"))
        (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
        (hsPkgs."process" or (errorHandler.buildDepError "process"))
        (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
        (hsPkgs."recursion-schemes" or (errorHandler.buildDepError
          "recursion-schemes"))
        (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
        (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
        (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
        (hsPkgs."stm-conduit" or (errorHandler.buildDepError "stm-conduit"))
        (hsPkgs."streams" or (errorHandler.buildDepError "streams"))
        (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
        (hsPkgs."template-haskell" or (errorHandler.buildDepError
          "template-haskell"))
        (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
        (hsPkgs."text" or (errorHandler.buildDepError "text"))
        (hsPkgs."these" or (errorHandler.buildDepError "these"))
        (hsPkgs."time" or (errorHandler.buildDepError "time"))
        (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
        (hsPkgs."unordered-containers" or (errorHandler.buildDepError
          "unordered-containers"))
        (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
        (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
        (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
        (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
      ];
      build-tools = [
        (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError
          "happy:happy")))
        (hsPkgs.buildPackages.alex.components.exes.alex or (pkgs.buildPackages.alex or (errorHandler.buildToolDepError
          "alex:alex")))
      ];
      buildable = true;
      modules = [
        "Changed"
        "Control/Monad/Counter"
        "Data/Graph/TopologicalSort"
        "Data/InternedText"
        "Data/Limit"
        "Data/Sup"
        "Debug"
        "ErrorContext"
        "From"
        "GlobalMain"
        "Injection"
        "Kore/AST/ApplicativeKore"
        "Kore/AST/AstWithLocation"
        "Kore/AST/Common"
        "Kore/AST/Error"
        "Kore/Attribute/Assoc"
        "Kore/Attribute/Attributes"
        "Kore/Attribute/Axiom"
        "Kore/Attribute/Axiom/Concrete"
        "Kore/Attribute/Axiom/Constructor"
        "Kore/Attribute/Axiom/NonExecutable"
        "Kore/Attribute/Axiom/Symbolic"
        "Kore/Attribute/Axiom/Unit"
        "Kore/Attribute/Comm"
        "Kore/Attribute/Constructor"
        "Kore/Attribute/Definition"
        "Kore/Attribute/Function"
        "Kore/Attribute/Functional"
        "Kore/Attribute/Hook"
        "Kore/Attribute/Idem"
        "Kore/Attribute/Injective"
        "Kore/Attribute/Label"
        "Kore/Attribute/Location"
        "Kore/Attribute/Null"
        "Kore/Attribute/Overload"
        "Kore/Attribute/Owise"
        "Kore/Attribute/Parser"
        "Kore/Attribute/Pattern/ConstructorLike"
        "Kore/Attribute/Pattern/Created"
        "Kore/Attribute/Pattern/Defined"
        "Kore/Attribute/Pattern/FreeVariables"
        "Kore/Attribute/Pattern/Function"
        "Kore/Attribute/Pattern/Functional"
        "Kore/Attribute/Pattern/Simplified"
        "Kore/Attribute/PredicatePattern"
        "Kore/Attribute/Priority"
        "Kore/Attribute/ProductionID"
        "Kore/Attribute/RuleIndex"
        "Kore/Attribute/Simplification"
        "Kore/Attribute/Smthook"
        "Kore/Attribute/SmtLemma"
        "Kore/Attribute/Smtlib"
        "Kore/Attribute/Smtlib/Smthook"
        "Kore/Attribute/Smtlib/Smtlib"
        "Kore/Attribute/Sort"
        "Kore/Attribute/Sort/Concat"
        "Kore/Attribute/Sort/Constructors"
        "Kore/Attribute/Sort/ConstructorsBuilder"
        "Kore/Attribute/Sort/Element"
        "Kore/Attribute/Sort/HasDomainValues"
        "Kore/Attribute/Sort/Unit"
        "Kore/Attribute/SortInjection"
        "Kore/Attribute/Source"
        "Kore/Attribute/SourceLocation"
        "Kore/Attribute/Subsort"
        "Kore/Attribute/Symbol"
        "Kore/Attribute/Symbol/Anywhere"
        "Kore/Attribute/Symbol/Klabel"
        "Kore/Attribute/Symbol/Memo"
        "Kore/Attribute/Symbol/NoEvaluators"
        "Kore/Attribute/Symbol/SymbolKywd"
        "Kore/Attribute/Synthetic"
        "Kore/Attribute/Trusted"
        "Kore/Attribute/UniqueId"
        "Kore/BugReport"
        "Kore/Builtin"
        "Kore/Builtin/AssocComm/AssocComm"
        "Kore/Builtin/AssocComm/CeilSimplifier"
        "Kore/Builtin/AssociativeCommutative"
        "Kore/Builtin/Attributes"
        "Kore/Builtin/Bool"
        "Kore/Builtin/Bool/Bool"
        "Kore/Builtin/Builtin"
        "Kore/Builtin/Encoding"
        "Kore/Builtin/Endianness"
        "Kore/Builtin/Endianness/Endianness"
        "Kore/Builtin/EqTerm"
        "Kore/Builtin/Error"
        "Kore/Builtin/Inj"
        "Kore/Builtin/Int"
        "Kore/Builtin/Int/Int"
        "Kore/Builtin/InternalBytes"
        "Kore/Builtin/InternalBytes/InternalBytes"
        "Kore/Builtin/IO"
        "Kore/Builtin/KEqual"
        "Kore/Builtin/Kreflection"
        "Kore/Builtin/Krypto"
        "Kore/Builtin/List"
        "Kore/Builtin/List/List"
        "Kore/Builtin/Map"
        "Kore/Builtin/Map/Map"
        "Kore/Builtin/Set"
        "Kore/Builtin/Set/Set"
        "Kore/Builtin/Signedness"
        "Kore/Builtin/Signedness/Signedness"
        "Kore/Builtin/String"
        "Kore/Builtin/String/String"
        "Kore/Builtin/Symbols"
        "Kore/Builtin/Verifiers"
        "Kore/Debug"
        "Kore/Equation"
        "Kore/Equation/Application"
        "Kore/Equation/DebugEquation"
        "Kore/Equation/Equation"
        "Kore/Equation/Registry"
        "Kore/Equation/Sentence"
        "Kore/Equation/Simplification"
        "Kore/Equation/Validate"
        "Kore/Error"
        "Kore/Exec"
        "Kore/Exec/GraphTraversal"
        "Kore/IndexedModule/Error"
        "Kore/IndexedModule/IndexedModule"
        "Kore/IndexedModule/MetadataTools"
        "Kore/IndexedModule/MetadataToolsBuilder"
        "Kore/IndexedModule/OverloadGraph"
        "Kore/IndexedModule/Resolvers"
        "Kore/IndexedModule/SortGraph"
        "Kore/Internal/Alias"
        "Kore/Internal/ApplicationSorts"
        "Kore/Internal/Condition"
        "Kore/Internal/Conditional"
        "Kore/Internal/From"
        "Kore/Internal/Inj"
        "Kore/Internal/InternalBool"
        "Kore/Internal/InternalBytes"
        "Kore/Internal/InternalInt"
        "Kore/Internal/InternalList"
        "Kore/Internal/InternalMap"
        "Kore/Internal/InternalSet"
        "Kore/Internal/InternalString"
        "Kore/Internal/Key"
        "Kore/Internal/MultiAnd"
        "Kore/Internal/MultiExists"
        "Kore/Internal/MultiOr"
        "Kore/Internal/NormalForm"
        "Kore/Internal/NormalizedAc"
        "Kore/Internal/OrCondition"
        "Kore/Internal/OrPattern"
        "Kore/Internal/Pattern"
        "Kore/Internal/Predicate"
        "Kore/Internal/SideCondition"
        "Kore/Internal/SideCondition/SideCondition"
        "Kore/Internal/Substitution"
        "Kore/Internal/Symbol"
        "Kore/Internal/TermLike"
        "Kore/Internal/TermLike/Renaming"
        "Kore/Internal/TermLike/TermLike"
        "Kore/Internal/Variable"
        "Kore/JsonRpc"
        "Kore/Log"
        "Kore/Log/DebugAppliedRewriteRules"
        "Kore/Log/DebugAttemptedRewriteRules"
        "Kore/Log/DebugBeginClaim"
        "Kore/Log/DebugCreatedSubstitution"
        "Kore/Log/DebugEvaluateCondition"
        "Kore/Log/DebugProven"
        "Kore/Log/DebugRetrySolverQuery"
        "Kore/Log/DebugRewriteTrace"
        "Kore/Log/DebugSolver"
        "Kore/Log/DebugSubstitutionSimplifier"
        "Kore/Log/DebugTransition"
        "Kore/Log/DebugUnification"
        "Kore/Log/DebugUnifyBottom"
        "Kore/Log/DecidePredicateUnknown"
        "Kore/Log/ErrorBottomTotalFunction"
        "Kore/Log/ErrorEquationRightFunction"
        "Kore/Log/ErrorEquationsSameMatch"
        "Kore/Log/ErrorException"
        "Kore/Log/ErrorOutOfDate"
        "Kore/Log/ErrorParse"
        "Kore/Log/ErrorRewriteLoop"
        "Kore/Log/ErrorRewritesInstantiation"
        "Kore/Log/ErrorVerify"
        "Kore/Log/InfoAttemptUnification"
        "Kore/Log/JsonRpc"
        "Kore/Log/InfoExecBreadth"
        "Kore/Log/InfoExecDepth"
        "Kore/Log/InfoJsonRpcCancelRequest"
        "Kore/Log/InfoJsonRpcProcessRequest"
        "Kore/Log/InfoProofDepth"
        "Kore/Log/InfoReachability"
        "Kore/Log/InfoUserLog"
        "Kore/Log/KoreLogOptions"
        "Kore/Log/Registry"
        "Kore/Log/SQLite"
        "Kore/Log/WarnBottom"
        "Kore/Log/WarnBoundedModelChecker"
        "Kore/Log/WarnDepthLimitExceeded"
        "Kore/Log/WarnFunctionWithoutEvaluators"
        "Kore/Log/WarnIfLowProductivity"
        "Kore/Log/WarnNotAPredicate"
        "Kore/Log/WarnNotImplemented"
        "Kore/Log/WarnRestartSolver"
        "Kore/Log/WarnStuckClaimState"
        "Kore/Log/WarnSymbolSMTRepresentation"
        "Kore/Log/WarnTrivialClaim"
        "Kore/Log/WarnUnexploredBranches"
        "Kore/Log/WarnUnsimplified"
        "Kore/ModelChecker/Bounded"
        "Kore/ModelChecker/Simplification"
        "Kore/ModelChecker/Step"
        "Kore/Network/JSONRPC"
        "Kore/Options"
        "Kore/Parser"
        "Kore/Parser/CString"
        "Kore/Parser/Lexer"
        "Kore/Parser/LexerWrapper"
        "Kore/Parser/Parser"
        "Kore/Parser/ParserUtils"
        "Kore/Reachability"
        "Kore/Reachability/AllPathClaim"
        "Kore/Reachability/Claim"
        "Kore/Reachability/ClaimState"
        "Kore/Reachability/OnePathClaim"
        "Kore/Reachability/Prim"
        "Kore/Reachability/Prove"
        "Kore/Reachability/SomeClaim"
        "Kore/Repl"
        "Kore/Repl/Data"
        "Kore/Repl/Interpreter"
        "Kore/Repl/Parser"
        "Kore/Repl/State"
        "Kore/Rewrite"
        "Kore/Rewrite/AntiLeft"
        "Kore/Rewrite/Axiom/EvaluationStrategy"
        "Kore/Rewrite/Axiom/Identifier"
        "Kore/Rewrite/Axiom/Matcher"
        "Kore/Rewrite/Axiom/MatcherData"
        "Kore/Rewrite/AxiomPattern"
        "Kore/Rewrite/ClaimPattern"
        "Kore/Rewrite/Function/Evaluator"
        "Kore/Rewrite/Function/Memo"
        "Kore/Rewrite/Implication"
        "Kore/Rewrite/Remainder"
        "Kore/Rewrite/Result"
        "Kore/Rewrite/RewriteStep"
        "Kore/Rewrite/Rule"
        "Kore/Rewrite/Rule/Expand"
        "Kore/Rewrite/Rule/Simplify"
        "Kore/Rewrite/RulePattern"
        "Kore/Rewrite/Search"
        "Kore/Rewrite/SMT/AST"
        "Kore/Rewrite/SMT/Declaration"
        "Kore/Rewrite/SMT/Encoder"
        "Kore/Rewrite/SMT/Evaluator"
        "Kore/Rewrite/SMT/Lemma"
        "Kore/Rewrite/SMT/Representation/All"
        "Kore/Rewrite/SMT/Representation/Resolve"
        "Kore/Rewrite/SMT/Representation/Sorts"
        "Kore/Rewrite/SMT/Representation/Symbols"
        "Kore/Rewrite/SMT/Resolvers"
        "Kore/Rewrite/SMT/Translate"
        "Kore/Rewrite/Step"
        "Kore/Rewrite/Strategy"
        "Kore/Rewrite/Substitution"
        "Kore/Rewrite/Transition"
        "Kore/Rewrite/RewritingVariable"
        "Kore/Rewrite/UnifyingRule"
        "Kore/Simplify/And"
        "Kore/Simplify/AndPredicates"
        "Kore/Simplify/AndTerms"
        "Kore/Simplify/API"
        "Kore/Simplify/Application"
        "Kore/Simplify/Bottom"
        "Kore/Simplify/Ceil"
        "Kore/Simplify/CeilSimplifier"
        "Kore/Simplify/Condition"
        "Kore/Simplify/DomainValue"
        "Kore/Simplify/Equals"
        "Kore/Simplify/Exists"
        "Kore/Simplify/ExpandAlias"
        "Kore/Simplify/Forall"
        "Kore/Simplify/Iff"
        "Kore/Simplify/Implies"
        "Kore/Simplify/In"
        "Kore/Simplify/Inhabitant"
        "Kore/Simplify/Inj"
        "Kore/Simplify/InjSimplifier"
        "Kore/Simplify/InternalBool"
        "Kore/Simplify/InternalBytes"
        "Kore/Simplify/InternalInt"
        "Kore/Simplify/InternalList"
        "Kore/Simplify/InternalMap"
        "Kore/Simplify/InternalSet"
        "Kore/Simplify/InternalString"
        "Kore/Simplify/Mu"
        "Kore/Simplify/Next"
        "Kore/Simplify/NoConfusion"
        "Kore/Simplify/Not"
        "Kore/Simplify/NotSimplifier"
        "Kore/Simplify/Nu"
        "Kore/Simplify/Or"
        "Kore/Simplify/OrPattern"
        "Kore/Simplify/Overloading"
        "Kore/Simplify/OverloadSimplifier"
        "Kore/Simplify/Pattern"
        "Kore/Simplify/Predicate"
        "Kore/Simplify/SetVariable"
        "Kore/Simplify/SimplificationType"
        "Kore/Simplify/Simplify"
        "Kore/Simplify/StringLiteral"
        "Kore/Simplify/SubstitutionSimplifier"
        "Kore/Simplify/TermLike"
        "Kore/Simplify/Top"
        "Kore/Simplify/Variable"
        "Kore/Sort"
        "Kore/Substitute"
        "Kore/Syntax"
        "Kore/Syntax/And"
        "Kore/Syntax/Application"
        "Kore/Syntax/Bottom"
        "Kore/Syntax/Ceil"
        "Kore/Syntax/Definition"
        "Kore/Syntax/DomainValue"
        "Kore/Syntax/Equals"
        "Kore/Syntax/Exists"
        "Kore/Syntax/Floor"
        "Kore/Syntax/Forall"
        "Kore/Syntax/Id"
        "Kore/Syntax/Iff"
        "Kore/Syntax/Implies"
        "Kore/Syntax/In"
        "Kore/Syntax/Inhabitant"
        "Kore/Syntax/Json"
        "Kore/Syntax/Json/Internal"
        "Kore/Syntax/Module"
        "Kore/Syntax/Mu"
        "Kore/Syntax/Next"
        "Kore/Syntax/Not"
        "Kore/Syntax/Nu"
        "Kore/Syntax/Or"
        "Kore/Syntax/Pattern"
        "Kore/Syntax/PatternF"
        "Kore/Syntax/Rewrites"
        "Kore/Syntax/Sentence"
        "Kore/Syntax/StringLiteral"
        "Kore/Syntax/Top"
        "Kore/Syntax/Variable"
        "Kore/TopBottom"
        "Kore/Unification/Procedure"
        "Kore/Unification/SubstitutionNormalization"
        "Kore/Unification/SubstitutionSimplifier"
        "Kore/Unification/UnifierT"
        "Kore/Unification/Unify"
        "Kore/Unification/NewUnifier"
        "Kore/Unparser"
        "Kore/Util/TSM"
        "Kore/Util/TSM/UnifyTag"
        "Kore/Validate/AliasVerifier"
        "Kore/Validate/AttributesVerifier"
        "Kore/Validate/DefinitionVerifier"
        "Kore/Validate/Error"
        "Kore/Validate/ModuleVerifier"
        "Kore/Validate/PatternVerifier"
        "Kore/Validate/PatternVerifier/PatternVerifier"
        "Kore/Validate/SentenceVerifier"
        "Kore/Validate/SortVerifier"
        "Kore/Validate/Verifier"
        "Kore/Variables/Binding"
        "Kore/Variables/Free"
        "Kore/Variables/Fresh"
        "Kore/Verified"
        "Kore/VersionInfo"
        "Log"
        "Log/Entry"
        "Logic"
        "Options/SMT"
        "Pair"
        "Partial"
        "Paths_kore"
        "Prelude/Kore"
        "Pretty"
        "Prof"
        "SMT"
        "SMT/AST"
        "SMT/SimpleSMT"
        "SQL"
        "SQL/ColumnDef"
        "SQL/Key"
        "SQL/Query"
        "SQL/SOP"
        "SQL/SQL"
        "Stats"
      ];
      hsSourceDirs = [ "src" "app/share" ];
    };
    exes = {
      "kore-exec" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."compact" or (errorHandler.buildDepError "compact"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/exec" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-rpc" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/rpc" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-format" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/format" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-parser" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/parser" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-prof" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."eventlog2speedscope" or (errorHandler.buildDepError
            "eventlog2speedscope"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/prof" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-repl" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/repl" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-match-disjunction" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/match-disjunction" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-check-functions" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/check-functions" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
      "kore-simplify" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
        ];
        buildable = true;
        hsSourceDirs = [ "app/simplify" ];
        mainPath = (([ "Main.hs" ]
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.4")
          "")
          ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "8.8")
          "") ++ [ "" ];
      };
    };
    tests = {
      "kore-test" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."timeit" or (errorHandler.buildDepError "timeit"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."co-log" or (errorHandler.buildDepError "co-log"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."compact" or (errorHandler.buildDepError "compact"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError
            "conduit-extra"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."decision-diagrams" or (errorHandler.buildDepError
            "decision-diagrams"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError
            "deriving-aeson"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fgl" or (errorHandler.buildDepError "fgl"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."ghc-events" or (errorHandler.buildDepError "ghc-events"))
          (hsPkgs."ghc-trace-events" or (errorHandler.buildDepError
            "ghc-trace-events"))
          (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
          (hsPkgs."graphviz" or (errorHandler.buildDepError "graphviz"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."json-rpc" or (errorHandler.buildDepError "json-rpc"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."logict" or (errorHandler.buildDepError "logict"))
          (hsPkgs."matrix" or (errorHandler.buildDepError "matrix"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError
            "mono-traversable"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError
            "optparse-applicative"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError
            "parser-combinators"))
          (hsPkgs."pqueue" or (errorHandler.buildDepError "pqueue"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError
            "prettyprinter"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError
            "recursion-schemes"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError
            "sqlite-simple"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stm-conduit" or (errorHandler.buildDepError "stm-conduit"))
          (hsPkgs."streams" or (errorHandler.buildDepError "streams"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError
            "template-haskell"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError
            "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."kore" or (errorHandler.buildDepError "kore"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError
            "quickcheck-instances"))
          (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
          (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
          (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError
            "tasty-hedgehog"))
          (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
          (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError
            "tasty-quickcheck"))
          (hsPkgs."tasty-test-reporter" or (errorHandler.buildDepError
            "tasty-test-reporter"))
        ];
        build-tools = [
          (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError
            "happy:happy")))
          (hsPkgs.buildPackages.alex.components.exes.alex or (pkgs.buildPackages.alex or (errorHandler.buildToolDepError
            "alex:alex")))
          (hsPkgs.buildPackages.tasty-discover.components.exes.tasty-discover or (pkgs.buildPackages.tasty-discover or (errorHandler.buildToolDepError
            "tasty-discover:tasty-discover")))
        ];
        buildable = true;
        modules = [
          "Driver"
          "Test/ConsistentKore"
          "Test/Data/Graph/TopologicalSort"
          "Test/Data/InternedText"
          "Test/Data/Limit"
          "Test/Data/Sup"
          "Test/Debug"
          "Test/Expect"
          "Test/Injection"
          "Test/Kore"
          "Test/Kore/AST/Common"
          "Test/Kore/Attribute/Assoc"
          "Test/Kore/Attribute/Axiom/Concrete"
          "Test/Kore/Attribute/Axiom/Symbolic"
          "Test/Kore/Attribute/Axiom/Unit"
          "Test/Kore/Attribute/Comm"
          "Test/Kore/Attribute/Constructor"
          "Test/Kore/Attribute/Function"
          "Test/Kore/Attribute/Functional"
          "Test/Kore/Attribute/Hook"
          "Test/Kore/Attribute/Idem"
          "Test/Kore/Attribute/Injective"
          "Test/Kore/Attribute/Label"
          "Test/Kore/Attribute/NonExecutable"
          "Test/Kore/Attribute/Overload"
          "Test/Kore/Attribute/Owise"
          "Test/Kore/Attribute/Parser"
          "Test/Kore/Attribute/Pattern/ConstructorLike"
          "Test/Kore/Attribute/Pattern/Defined"
          "Test/Kore/Attribute/Pattern/FreeVariables"
          "Test/Kore/Attribute/Pattern/Function"
          "Test/Kore/Attribute/Pattern/Functional"
          "Test/Kore/Attribute/Pattern/Sort"
          "Test/Kore/Attribute/Priority"
          "Test/Kore/Attribute/ProductionID"
          "Test/Kore/Attribute/Simplification"
          "Test/Kore/Attribute/Smtlib"
          "Test/Kore/Attribute/Sort/ConstructorsBuilder"
          "Test/Kore/Attribute/Sort/HasDomainValues"
          "Test/Kore/Attribute/Sort/Unit"
          "Test/Kore/Attribute/SortInjection"
          "Test/Kore/Attribute/Subsort"
          "Test/Kore/Attribute/Symbol"
          "Test/Kore/Attribute/Symbol/Anywhere"
          "Test/Kore/Attribute/Symbol/Klabel"
          "Test/Kore/Attribute/Symbol/Memo"
          "Test/Kore/Attribute/Symbol/NoEvaluators"
          "Test/Kore/Attribute/Symbol/SymbolKywd"
          "Test/Kore/Attribute/Trusted"
          "Test/Kore/Attribute/UniqueId"
          "Test/Kore/BugReport"
          "Test/Kore/Builtin"
          "Test/Kore/Builtin/AssocComm/CeilSimplifier"
          "Test/Kore/Builtin/Bool"
          "Test/Kore/Builtin/Builtin"
          "Test/Kore/Builtin/Definition"
          "Test/Kore/Builtin/Encoding"
          "Test/Kore/Builtin/Endianness"
          "Test/Kore/Builtin/External"
          "Test/Kore/Builtin/Inj"
          "Test/Kore/Builtin/Int"
          "Test/Kore/Builtin/InternalBytes"
          "Test/Kore/Builtin/IO"
          "Test/Kore/Builtin/KEqual"
          "Test/Kore/Builtin/Krypto"
          "Test/Kore/Builtin/List"
          "Test/Kore/Builtin/Map"
          "Test/Kore/Builtin/Set"
          "Test/Kore/Builtin/Signedness"
          "Test/Kore/Builtin/String"
          "Test/Kore/Contains"
          "Test/Kore/Equation/Application"
          "Test/Kore/Equation/Common"
          "Test/Kore/Equation/Sentence"
          "Test/Kore/Equation/Simplification"
          "Test/Kore/Error"
          "Test/Kore/Exec"
          "Test/Kore/IndexedModule/Error"
          "Test/Kore/IndexedModule/MockMetadataTools"
          "Test/Kore/IndexedModule/OverloadGraph"
          "Test/Kore/IndexedModule/Resolvers"
          "Test/Kore/IndexedModule/SortGraph"
          "Test/Kore/Internal/ApplicationSorts"
          "Test/Kore/Internal/Condition"
          "Test/Kore/Internal/From"
          "Test/Kore/Internal/Key"
          "Test/Kore/Internal/MultiAnd"
          "Test/Kore/Internal/MultiExists"
          "Test/Kore/Internal/OrCondition"
          "Test/Kore/Internal/OrPattern"
          "Test/Kore/Internal/Pattern"
          "Test/Kore/Internal/Predicate"
          "Test/Kore/Internal/SideCondition"
          "Test/Kore/Internal/Substitution"
          "Test/Kore/Internal/Symbol"
          "Test/Kore/Internal/TermLike"
          "Test/Kore/Log/DebugEvaluateCondition"
          "Test/Kore/Log/ErrorBottomTotalFunction"
          "Test/Kore/Log/WarnFunctionWithoutEvaluators"
          "Test/Kore/Log/WarnSymbolSMTRepresentation"
          "Test/Kore/Options"
          "Test/Kore/Parser"
          "Test/Kore/Parser/Lexer"
          "Test/Kore/Parser/Parser"
          "Test/Kore/Reachability/Claim"
          "Test/Kore/Reachability/MockAllPath"
          "Test/Kore/Reachability/OnePathStrategy"
          "Test/Kore/Reachability/Prove"
          "Test/Kore/Reachability/SomeClaim"
          "Test/Kore/Repl/Graph"
          "Test/Kore/Repl/Interpreter"
          "Test/Kore/Repl/Parser"
          "Test/Kore/Repl/ParserTest"
          "Test/Kore/Rewrite"
          "Test/Kore/Rewrite/AntiLeft"
          "Test/Kore/Rewrite/Axiom/EvaluationStrategy"
          "Test/Kore/Rewrite/Axiom/Identifier"
          "Test/Kore/Rewrite/Axiom/Matcher"
          "Test/Kore/Rewrite/Axiom/Registry"
          "Test/Kore/Rewrite/ClaimPattern"
          "Test/Kore/Rewrite/Function/Evaluator"
          "Test/Kore/Rewrite/Function/Integration"
          "Test/Kore/Rewrite/Function/Memo"
          "Test/Kore/Rewrite/Implication"
          "Test/Kore/Rewrite/MockSymbols"
          "Test/Kore/Rewrite/Remainder"
          "Test/Kore/Rewrite/RewriteStep"
          "Test/Kore/Rewrite/Rule"
          "Test/Kore/Rewrite/Rule/Common"
          "Test/Kore/Rewrite/Rule/Expand"
          "Test/Kore/Rewrite/Rule/Simplify"
          "Test/Kore/Rewrite/RulePattern"
          "Test/Kore/Rewrite/SMT/Builders"
          "Test/Kore/Rewrite/SMT/Evaluator"
          "Test/Kore/Rewrite/SMT/Helpers"
          "Test/Kore/Rewrite/SMT/Representation/All"
          "Test/Kore/Rewrite/SMT/Representation/Builders"
          "Test/Kore/Rewrite/SMT/Representation/Helpers"
          "Test/Kore/Rewrite/SMT/Representation/Sorts"
          "Test/Kore/Rewrite/SMT/Representation/Symbols"
          "Test/Kore/Rewrite/SMT/Sorts"
          "Test/Kore/Rewrite/SMT/Symbols"
          "Test/Kore/Rewrite/SMT/Translate"
          "Test/Kore/Rewrite/Strategy"
          "Test/Kore/Rewrite/Transition"
          "Test/Kore/Rewrite/RewritingVariable"
          "Test/Kore/Simplify"
          "Test/Kore/Simplify/And"
          "Test/Kore/Simplify/AndTerms"
          "Test/Kore/Simplify/Application"
          "Test/Kore/Simplify/Bottom"
          "Test/Kore/Simplify/Ceil"
          "Test/Kore/Simplify/Condition"
          "Test/Kore/Simplify/DomainValue"
          "Test/Kore/Simplify/Equals"
          "Test/Kore/Simplify/Exists"
          "Test/Kore/Simplify/Forall"
          "Test/Kore/Simplify/Iff"
          "Test/Kore/Simplify/Implies"
          "Test/Kore/Simplify/Inj"
          "Test/Kore/Simplify/InjSimplifier"
          "Test/Kore/Simplify/Integration"
          "Test/Kore/Simplify/IntegrationProperty"
          "Test/Kore/Simplify/InternalList"
          "Test/Kore/Simplify/InternalMap"
          "Test/Kore/Simplify/InternalSet"
          "Test/Kore/Simplify/Next"
          "Test/Kore/Simplify/Not"
          "Test/Kore/Simplify/Or"
          "Test/Kore/Simplify/OrPattern"
          "Test/Kore/Simplify/Overloading"
          "Test/Kore/Simplify/Predicate"
          "Test/Kore/Simplify/Pattern"
          "Test/Kore/Simplify/StringLiteral"
          "Test/Kore/Simplify/SubstitutionSimplifier"
          "Test/Kore/Simplify/TermLike"
          "Test/Kore/Simplify/Top"
          "Test/Kore/Syntax/Id"
          "Test/Kore/Syntax/Json"
          "Test/Kore/Syntax/Json/Roundtrips"
          "Test/Kore/Syntax/Variable"
          "Test/Kore/TopBottom"
          "Test/Kore/Unification/SubstitutionNormalization"
          "Test/Kore/Unification/Unifier"
          "Test/Kore/Unification/UnifierT"
          "Test/Kore/Unparser"
          "Test/Kore/Validate/DefinitionVerifier"
          "Test/Kore/Validate/DefinitionVerifier/Imports"
          "Test/Kore/Validate/DefinitionVerifier/PatternVerifier"
          "Test/Kore/Validate/DefinitionVerifier/SentenceVerifier"
          "Test/Kore/Validate/DefinitionVerifier/SortUsage"
          "Test/Kore/Validate/DefinitionVerifier/UniqueNames"
          "Test/Kore/Validate/DefinitionVerifier/UniqueSortVariables"
          "Test/Kore/Variables/Fresh"
          "Test/Kore/Variables/V"
          "Test/Kore/Variables/W"
          "Test/Kore/With"
          "Test/Pretty"
          "Test/SMT"
          "Test/SMT/AST"
          "Test/SQL"
          "Test/Stats"
          "Test/Tasty/HUnit/Ext"
          "Test/Terse"
        ];
        hsSourceDirs = [ "test" ];
        mainPath = [ "Test.hs" ];
      };
    };
  };
} // rec {
  src = (pkgs.lib).mkDefault ./kore;
}
