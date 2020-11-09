{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

This module contains parsers for Kore patterns, sentences, modules, and
definitions. We assume several conventions:

1.  All parsers expect no leading whitespace and consume all trailing
    whitespace. (See "Kore.Parser.Lexer" for whitespace definitions.)
2.  Parsers named @Remainder@ assume that their prefix (usually, an identifier)
    has already been parsed.

 -}

module Kore.Parser.Parser
    ( parseDefinition, parseDefinitionAux
    , parseModule, parseModuleAux
    , parsePattern
    , embedParsedPattern
    , parseAliasHead, parseSymbolHead
    , parseSortVariable
    , parseSort
    , parseAttributes
    , parseSentence
    , parseElementVariable
    , parseSetVariable
    , parseVariableCounter
    ) where

import Prelude.Kore hiding
    ( many
    , some
    )

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import Text.Megaparsec
    ( many
    , some
    , (<?>)
    )
import qualified Text.Megaparsec as Parse

import Data.Sup
import Kore.Parser.Lexeme
import Kore.Parser.ParserUtils
    ( Parser
    )
import Kore.Syntax
import Kore.Syntax.Definition
import Kore.Unparser
    ( unparse
    )
import Numeric.Natural

embedParsedPattern :: (PatternF VariableName) ParsedPattern -> ParsedPattern
embedParsedPattern patternBase = asPattern (mempty :< patternBase)

{- | Parses a @sort-variable@.

@
<sort-variable> ::= <sort-id>
@
-}
parseSortVariable :: Parser SortVariable
parseSortVariable = SortVariable <$> parseSortId

{- | Parses a @sort@.

@
<sort> ::= <sort-variable> | <sort-id> "{" <sorts> "}"
@
-}
parseSort :: Parser Sort
parseSort =
    (parseSortId >>= parseRemainder) <?> "sort"
  where
    parseRemainder ident =
        (SortActualSort <$> parseSortActualRemainder ident)
        <|> (SortVariableSort <$> parseSortVariableRemainder ident)

parseSortActualRemainder :: Id -> Parser SortActual
parseSortActualRemainder sortActualName = do
    sortActualSorts <- braces . list $ parseSort
    pure SortActual { sortActualName, sortActualSorts }

parseSortVariableRemainder :: Id -> Parser SortVariable
parseSortVariableRemainder = pure . SortVariable

{- | Parses the head of a symbol or alias declaration.

@
<symbol-or-alias> ::= <symbol-id> "{" <sort-variables> "}"
@

-}
parseSymbolOrAliasDeclarationHead
    :: (Id -> [SortVariable] -> head)  -- ^ head constructor
    -> Parser head
parseSymbolOrAliasDeclarationHead mkHead = do
    identifier <- parseSymbolId
    parameters <- braces . list $ parseSortVariable
    pure (mkHead identifier parameters)

{- | Parses @symbol-or-alias@ and interprets it as an 'Alias'.

@
<alias> ::= <symbol-or-alias>
@

 -}
parseAliasHead :: Parser Alias
parseAliasHead = parseSymbolOrAliasDeclarationHead Alias


{- | Parses @symbol-or-alias@ and interprets it as a 'Symbol'.

@
<symbol> ::= <symbol-or-alias>
@

 -}
parseSymbolHead :: Parser Symbol
parseSymbolHead = parseSymbolOrAliasDeclarationHead Symbol

{-| Parses an pattern.

@
<pattern>
  ::= <element-variable>
    | <set-variable>
    | <ML-pattern>
    | <application-pattern>
    | <string-literal>
@
-}
parsePattern :: Parser ParsedPattern
parsePattern =
    (embedParsedPattern <$> parseLiteral) <|> (parseAnyId >>= parseRemainder)
  where
    parseRemainder identifier =
        parseVariableRemainder identifier
        <|> parseKoreRemainder identifier
        <|> (parseApplicationRemainder identifier)

parseLiteral :: Parser (PatternF VariableName ParsedPattern)
parseLiteral =
    (StringLiteralF . Const <$> parseStringLiteral)
    <?> "string literal"

parseVariable :: Parser (SomeVariable VariableName)
parseVariable = do
    variableName <- parseAnyId >>= getSomeVariableName
    colon
    variableSort <- parseSort
    pure Variable { variableName, variableSort }

{- | Parse a variable, given that the identifier is already parsed.

@
<variable> ::= <element-variable> | <set-variable>
@

Set variables always start with @\@@, while element variables do not.
-}
parseVariableRemainder :: Id -> Parser ParsedPattern
parseVariableRemainder identifier = do
    -- Before we see the 'colon' token, we don't know if the identifier should
    -- refer to a symbol or a variable.
    colon
    -- After the 'colon' token, we know that the identifier must refer to a
    -- variable, not a symbol, and now we will validate it as a variable name.
    variableName <- getSomeVariableName identifier
    variableSort <- parseSort
    (pure . embedParsedPattern . VariableF . Const)
        Variable { variableName, variableSort }

getSomeVariableName :: Id -> Parser (SomeVariableName VariableName)
getSomeVariableName identifier =
    (inject <$> getSetVariableName identifier)
    <|> (inject <$> getElementVariableName identifier)
    <|> expectedVariableName
  where
    expectedVariableName =
        (fail . unwords)
        [ "expected a variable name, but found:"
        , (show . unparse) identifier
        ]

getSetVariableName :: Id -> Parser (SetVariableName VariableName)
getSetVariableName identifier
  | isSetVariableId identifier =
    pure (SetVariableName (getVariableName identifier))
  | otherwise = empty

{- | Parse a set variable.

@
<set-variable> ::= <set-variable-id> ":" <sort>
@
-}
parseSetVariable :: Parser (SetVariable VariableName)
parseSetVariable = do
    variableName <- parseSetId >>= getSetVariableName
    colon
    variableSort <- parseSort
    pure Variable { variableName, variableSort }

getElementVariableName :: Id -> Parser (ElementVariableName VariableName)
getElementVariableName identifier
  | isElementVariableId identifier =
    pure (ElementVariableName (getVariableName identifier))
  | otherwise = empty

{- | Parse an element variable.

@
<element-variable> ::= <element-variable-id> ":" <sort>
@
-}
parseElementVariable :: Parser (ElementVariable VariableName)
parseElementVariable = do
    variableName <- parseId >>= getElementVariableName
    colon
    variableSort <- parseSort
    pure Variable { variableName, variableSort }

{- | Parse an entire 'SymbolOrAlias' occurring in a pattern.
 -}
parseSymbolOrAlias :: Parser SymbolOrAlias
parseSymbolOrAlias = parseSymbolId >>= parseSymbolOrAliasRemainder

{- | Parse an entire 'Application' occurring in a pattern.

@
<application-pattern> ::= <symbol-id> "{" <sorts> "}" "(" <patterns> ")"
@
 -}
parseApplication
    :: Parser child
    -> Parser (Application SymbolOrAlias child)
parseApplication parseChild = do
    applicationSymbolOrAlias <- parseSymbolOrAlias
    applicationChildren <- parens . list $ parseChild
    pure Application { applicationSymbolOrAlias, applicationChildren }

{- | Parse the tail of an 'Application' pattern, after the @Id@.

@
... "{" <sorts> "}" "(" <patterns> ")"
@
 -}
parseApplicationRemainder :: Id -> Parser ParsedPattern
parseApplicationRemainder identifier = do
    applicationSymbolOrAlias <- parseSymbolOrAliasRemainder identifier
    applicationChildren <- parens . list $ parsePattern
    (pure . embedParsedPattern . ApplicationF)
        Application { applicationSymbolOrAlias, applicationChildren }

{- | Parse the tail of a 'SymbolOrAlias', after the @Id@.

@
... "{" <sorts> "}"
@
 -}
parseSymbolOrAliasRemainder :: Id -> Parser SymbolOrAlias
parseSymbolOrAliasRemainder symbolOrAliasConstructor = do
    Monad.guard (isSymbolId symbolOrAliasConstructor)
    symbolOrAliasParams <- braces . list $ parseSort
    pure SymbolOrAlias { symbolOrAliasConstructor, symbolOrAliasParams }

{- | Parse the @\\left-assoc@ syntactic sugar.

@
"\\left-assoc" '{' '}' '(' <application-pattern> ')'
@
 -}
parseLeftAssoc :: Parser ParsedPattern
parseLeftAssoc = do
    braces $ pure ()
    application <- parens $ parseApplication parsePattern
    let mkApplication child1 child2 =
            application { applicationChildren = [child1, child2] }
            & ApplicationF
            & embedParsedPattern
    case applicationChildren application of
        [] -> fail "expected one or more arguments"
        children -> pure (foldl1 mkApplication children)

{- | Parse a built-in Kore (matching logic) pattern.

@
<ML-pattern>
  ::=
    // Connectives
      "\top" "{" <sort> "}" "(" ")"
    | "\bottom" "{" <sort> "}" "(" ")"
    | "\not" "{" <sort> "}" "(" <pattern> ")"
    | "\and" "{" <sort> "}" "(" <pattern> "," <pattern> ")"
    | "\or" "{" <sort> "}" "(" <pattern> "," <pattern> ")"
    | "\implies" "{" <sort> "}" "(" <pattern> "," <pattern> ")"
    | "\iff" "{" <sort> "}" "(" <pattern> "," <pattern> ")"

    // Quantifiers
    | "\exists" "{" <sort> "}" "(" <element-variable> "," <pattern> ")"
    | "\forall" "{" <sort> "}" "(" <element-variable> "," <pattern> ")"

    // Fixpoints
    | "\mu" "(" <set-variable> "," <pattern> ")"
    | "\nu" "(" <set-variable> "," <pattern> ")"

    // Predicates
    | "\ceil" "{" <sort> "," <sort> "}" "(" <pattern> ")"
    | "\floor" "{" <sort> "," <sort> "}" "(" <pattern> ")"
    | "\equals" "{" <sort> "," <sort> "}" "(" <pattern> "," <pattern> ")"
    | "\in" "{" <sort> "," <sort> "}" "(" <pattern> "," <pattern> ")"

    // Rewriting
    | "\next" "{" <sort> "}" "(" <pattern> ")"
    | "\rewrites" "{" <sort> "}" "(" <pattern> "," <pattern> ")"

    // Values
    | "\dv" "{" <sort> "}" "(" <string-literal> ")"

    // Syntax sugar
    | "\left-assoc" "{" "}" "(" <application-pattern> ")"
@

Always starts with @\@.
-}
parseKoreRemainder :: Id -> Parser ParsedPattern
parseKoreRemainder identifier =
    getSpecialId identifier >>= \case
        -- Connectives
        "top" -> embedParsedPattern . TopF <$> parseConnective0 Top
        "bottom" -> embedParsedPattern . BottomF <$> parseConnective0 Bottom
        "not" -> embedParsedPattern . NotF <$> parseConnective1 Not
        "and" -> embedParsedPattern . AndF <$> parseConnective2 And
        "or" -> embedParsedPattern . OrF <$> parseConnective2 Or
        "implies" -> embedParsedPattern . ImpliesF <$> parseConnective2 Implies
        "iff" -> embedParsedPattern . IffF <$> parseConnective2 Iff
        -- Quantifiers
        "exists" -> embedParsedPattern . ExistsF <$> parseQuantifier Exists
        "forall" -> embedParsedPattern . ForallF <$> parseQuantifier Forall
        -- Fixpoints
        "mu" -> embedParsedPattern . MuF <$> parseFixpoint Mu
        "nu" -> embedParsedPattern . NuF <$> parseFixpoint Nu
        -- Predicates
        "ceil" -> embedParsedPattern . CeilF <$> parsePredicate1 Ceil
        "floor" -> embedParsedPattern . FloorF <$> parsePredicate1 Floor
        "equals" -> embedParsedPattern . EqualsF <$> parsePredicate2 Equals
        "in" -> embedParsedPattern . InF <$> parsePredicate2 In
        -- Rewriting
        "next" -> embedParsedPattern . NextF <$> parseConnective1 Next
        "rewrites" -> embedParsedPattern . RewritesF <$> parseConnective2 Rewrites
        -- Values
        "dv" -> embedParsedPattern . DomainValueF <$> parseDomainValue
        -- Syntax sugar
        "left-assoc" -> parseLeftAssoc

        _ -> empty

getSpecialId :: Id -> Parser Text
getSpecialId Id { getId } = do
    Monad.guard (Text.head getId == '\\')
    pure (Text.tail getId)

{- | Parse a 0-argument connective.

@
_ ::= _ "{" ⟨sort⟩ "}" "(" ")"
@
 -}
parseConnective0 :: (Sort -> result) -> Parser result
parseConnective0 mkResult = do
    sort <- braces parseSort
    () <- parens $ pure ()
    pure (mkResult sort)

{- | Parse a 1-argument connective.

@
_ ::= _ "{" ⟨sort⟩ "}" "(" ⟨pattern⟩ ")"
@
 -}
parseConnective1 :: (Sort -> ParsedPattern -> result) -> Parser result
parseConnective1 mkResult = do
    sort <- braces parseSort
    child <- parens parsePattern
    pure (mkResult sort child)

{- | Parse a 2-argument connective.

@
_ ::= _ "{" ⟨sort⟩ "}" "(" ⟨pattern⟩ "," ⟨pattern⟩ ")"
@
 -}
parseConnective2
    :: (Sort -> ParsedPattern -> ParsedPattern -> result)
    -> Parser result
parseConnective2 mkResult = do
    sort <- braces parseSort
    (child1, child2) <- parens . pair $ parsePattern
    pure (mkResult sort child1 child2)

{- | Parse a quantifier.

@
_ ::= _ "{" ⟨sort⟩ "}" "(" ⟨element-variable⟩ "," ⟨pattern⟩ ")"
@
 -}
parseQuantifier
    :: (Sort -> ElementVariable VariableName -> ParsedPattern -> result)
    -> Parser result
parseQuantifier mkResult = do
    sort <- braces parseSort
    (variable, child) <- parensTuple parseElementVariable parsePattern
    pure (mkResult sort variable child)

{- | Parse a fixpoint.

@
_ ::= _ "{" ⟨sort⟩ "}" "(" ⟨set-variable⟩ "," ⟨pattern⟩ ")"
@
 -}
parseFixpoint
    :: (SetVariable VariableName -> ParsedPattern -> result)
    -> Parser result
parseFixpoint mkResult = do
    () <- braces $ pure ()
    (variable, child) <- parensTuple parseSetVariable parsePattern
    pure (mkResult variable child)

{- | Parse a 1-argument predicate.

@
_ ::= _ "{" ⟨sort⟩ "," ⟨sort⟩ "}" "(" ⟨pattern⟩ ")"
@
 -}
parsePredicate1
    :: (Sort -> Sort -> ParsedPattern -> result)
    -> Parser result
parsePredicate1 mkResult = do
    (sort1, sort2) <- bracesPair parseSort
    child <- parens parsePattern
    pure (mkResult sort1 sort2 child)

{- | Parse a 2-argument predicate.

@
_ ::= _ "{" ⟨sort⟩ "," ⟨sort⟩ "}" "(" ⟨pattern⟩ "," ⟨pattern⟩ ")"
@
 -}
parsePredicate2
    :: (Sort -> Sort -> ParsedPattern -> ParsedPattern -> result)
    -> Parser result
parsePredicate2 mkResult = do
    (sort1, sort2) <- bracesPair parseSort
    (child1, child2) <- parensPair parsePattern
    pure (mkResult sort1 sort2 child1 child2)

{- | Get a 'VariableName' from an 'Id'.

Uses 'parseVariableCounter' to get the 'counter' from the 'Id', if any.

 -}
getVariableName :: Id -> VariableName
getVariableName identifier =
    let (base, counter) = parseVariableCounter identifier
    in VariableName { base, counter }

{- | Read the fresh name counter (if any) from the end of an 'Id'.
 -}
parseVariableCounter :: Id -> (Id, Maybe (Sup Natural))
parseVariableCounter identifier@Id { getId, idLocation }
  -- Cases:
  -- suffix is empty: no counter, Id is not changed
  | Text.null suffix = (identifier, Nothing)
  -- suffix is all zeros: counter is zero, Id has final zero stripped
  | Text.null nonZeros =
    ( Id { idLocation, getId = base <> Text.init zeros }
    , Just (Element 0)
    )
  -- suffix is some zeros followed by non-zeros:
  --   read the counter from the non-zeros, Id is base + zeros
  | otherwise =
    ( Id { idLocation, getId = base <> zeros }
    , (Just . Element) (read $ Text.unpack nonZeros)
    )
  where
    base = Text.dropWhileEnd Char.isDigit getId
    suffix = Text.drop (Text.length base) getId
    zeros = Text.takeWhile (== '0') suffix
    nonZeros = Text.drop (Text.length zeros) suffix

{- | Parse a 'DomainValue' pattern.

@
"\dv" "{" <sort> "}" "(" <string-literal> ")"
@
 -}
parseDomainValue :: Parser (DomainValue Sort ParsedPattern)
parseDomainValue =
    DomainValue <$> braces parseSort <*> parens parseChild
  where
    parseChild =
        embedParsedPattern . StringLiteralF . Const <$> parseStringLiteral

{- | Parse a comma-separated list of attributes in brackets.

@
⟨attributes⟩ ::= ‘[’ ⟨patterns⟩ ‘]’
@
 -}
parseAttributes :: Parser Attributes
parseAttributes =
    Attributes <$> brackets (Parse.sepBy parsePattern comma)

{- | Parse a Kore definition.

@
<definition>
  ::= <attributes> <modules_+>
@
-}
parseDefinition :: Parser ParsedDefinition
parseDefinition = parseDefinitionAux parseSentence

parseDefinitionAux
    :: Parser sentence
    -> Parser (Definition sentence)
parseDefinitionAux parseSentence' =
    Definition
        <$> parseAttributes
        <*> some (parseModuleAux parseSentence')

{- | Parse a Kore module.

@
<module>
  ::=   "module" <module-name-id>
            <sentences_>
        "endmodule"
        "[" <attributes> "]"
@
-}
parseModule :: Parser ParsedModule
parseModule = parseModuleAux parseSentence

parseModuleAux
    :: Parser sentence
    -> Parser (Module sentence)
parseModuleAux parseSentence' = do
    keyword "module"
    moduleName <- parseModuleName
    moduleSentences <- many parseSentence'
    keyword "endmodule"
    moduleAttributes <- parseAttributes
    return Module
           { moduleName
           , moduleSentences
           , moduleAttributes
           }

{- | Parse a Kore sentence.

@
<sentence>
  ::= <import-statement>
    | <sort-definition>
    | <hooked-sort-definition>
    | <symbol-definition>
    | <hooked-symbol-definition>
    | <axiom>
    | <claim>
    | <alias-definition>

<alias-definition> ::= "alias" ...
<axiom> ::= "axiom" ...
<claim> ::= "claim" ...
<sort-definition> ::= "sort" ...
<symbol-definition> ::= "symbol" ...
<import-statement> ::= "import" ...
<hooked-sort-definition> ::= "hooked-sort" ...
<hooked-symbol-definition> ::= "hooked-symbol" ...
@
 -}
parseSentence :: Parser ParsedSentence
parseSentence =
    parseSentenceImport
    <|> parseSentenceAlias
    <|> parseSentenceAxiom
    <|> parseSentenceClaim
    <|> parseSentenceSort
    <|> parseSentenceHookedSort
    <|> parseSentenceSymbol
    <|> parseSentenceHookedSymbol
    <?> "sentence"

{- | Parse a symbol declaration.

@
<symbol-definition>
  ::= "symbol" <symbol-id> "{" <sort-variables> "}"
        "(" <sorts> ")" ":" <sort>
        "[" <attributes> "]"
@
 -}
parseSentenceSymbol :: Parser ParsedSentence
parseSentenceSymbol = do
    keyword "symbol"
    SentenceSymbolSentence <$> parseSentenceSymbolRemainder

{- | Parse a hooked symbol declaration.

@
<hooked-symbol-definition>
  ::= "hooked-symbol" <symbol-id> "{" <sort-variables> "}"
        "(" <sorts> ")" ":" <sort>
        "[" <attributes> "]"
@
 -}
parseSentenceHookedSymbol :: Parser ParsedSentence
parseSentenceHookedSymbol = do
    keyword "hooked-symbol"
    SentenceHookSentence . SentenceHookedSymbol
        <$> parseSentenceSymbolRemainder

{- | Parse a @symbol@ or @hooked-symbol@ sentence after the initial keyword.

@
_
  ::= _ <symbol-id> "{" <sort-variables> "}"
        "(" <sorts> ")" ":" <sort>
        "[" <attributes> "]"
@

-}
parseSentenceSymbolRemainder :: Parser (SentenceSymbol ParsedPattern)
parseSentenceSymbolRemainder = do
    sentenceSymbolSymbol <- parseSymbolHead
    sentenceSymbolSorts <- parens . list $ parseSort
    colon
    sentenceSymbolResultSort <- parseSort
    sentenceSymbolAttributes <- parseAttributes
    pure SentenceSymbol
        { sentenceSymbolSymbol
        , sentenceSymbolSorts
        , sentenceSymbolResultSort
        , sentenceSymbolAttributes
        }

{- | Parse an alias definition

@
<alias-definition>
  ::= "alias" <symbol-id> "{" <sort-variables> "}"
        "(" <sorts> ")" ":" <sort>
        "where" <application-pattern> ":=" <pattern>
        "[" <attributes> "]"
@
-}
parseSentenceAlias :: Parser ParsedSentence
parseSentenceAlias = do
    keyword "alias"
    sentenceAliasAlias <- parseAliasHead
    sentenceAliasSorts <- parens . list $ parseSort
    colon
    sentenceAliasResultSort <- parseSort
    keyword "where"
    -- Note: constraints for left pattern checked in verifySentence
    sentenceAliasLeftPattern <- parseApplication parseVariable
    keyword ":="
    sentenceAliasRightPattern <- parsePattern
    sentenceAliasAttributes <- parseAttributes
    (pure . SentenceAliasSentence) SentenceAlias
        { sentenceAliasAlias
        , sentenceAliasSorts
        , sentenceAliasResultSort
        , sentenceAliasLeftPattern
        , sentenceAliasRightPattern
        , sentenceAliasAttributes
        }

{- | Parse an import declaration.

@
<import-statement>
  ::= "import" <module-name-id>
        "[" <attributes> "]"
@
-}
parseSentenceImport :: Parser ParsedSentence
parseSentenceImport = do
    keyword "import"
    sentenceImportModuleName <- parseModuleName
    sentenceImportAttributes <- parseAttributes
    (pure . SentenceImportSentence) SentenceImport
        { sentenceImportModuleName
        , sentenceImportAttributes
        }

{- | Parse an axiom sentence.

@
<axiom>
  ::= "axiom" "{" <sort-variables> "}"
        <pattern>
        "[" <attributes> "]"
@

 -}
parseSentenceAxiom :: Parser ParsedSentence
parseSentenceAxiom = do
    keyword "axiom"
    SentenceAxiomSentence <$> parseSentenceAxiomRemainder

{- | Parse an claim sentence.

@
<claim>
  ::= "claim" "{" <sort-variables> "}"
        <pattern>
        "[" <attributes> "]"
@

 -}
parseSentenceClaim :: Parser ParsedSentence
parseSentenceClaim = do
    keyword "claim"
    SentenceClaimSentence . SentenceClaim <$> parseSentenceAxiomRemainder

{- | Parses the part of @axiom@ or @claim@ after its introductory
keyword using the given constructor to construct the appropriate object.

@
_ ::= _ "{" <sort-variables> "}" <pattern> "[" <attributes> "]"
@

-}
parseSentenceAxiomRemainder :: Parser ParsedSentenceAxiom
parseSentenceAxiomRemainder = do
    sentenceAxiomParameters <- braces . list $ parseSortVariable
    sentenceAxiomPattern <- parsePattern
    sentenceAxiomAttributes <- parseAttributes
    pure SentenceAxiom
        { sentenceAxiomParameters
        , sentenceAxiomPattern
        , sentenceAxiomAttributes
        }

{- | Parse a sort sentence.

@
<sort-definition>
  ::= "sort" <sort-id> "{" <sort-variables> "}"
        "[" <attributes> "]"
@
-}
parseSentenceSort :: Parser ParsedSentence
parseSentenceSort = do
    keyword "sort"
    SentenceSortSentence <$> parseSentenceSortRemainder

{- | Parse a hooked sort sentence.

@
<hooked-sort-definition>
  ::= "hooked-sort" <sort-id> "{" <sort-variables> "}"
        "[" <attributes> "]"
@
-}
parseSentenceHookedSort :: Parser ParsedSentence
parseSentenceHookedSort = do
    keyword "hooked-sort"
    SentenceHookSentence . SentenceHookedSort <$> parseSentenceSortRemainder

{- | Parse the part of @sort@ or @hooked-sort@ after the initial keyword.

@
_ ::= _ <sort-id> "{" <sort-variables> "}" "[" <attributes> "]"
@

-}
parseSentenceSortRemainder :: Parser ParsedSentenceSort
parseSentenceSortRemainder = do
    sentenceSortName <- parseSortId
    sentenceSortParameters <- braces . list $ parseSortVariable
    sentenceSortAttributes <- parseAttributes
    pure SentenceSort
        { sentenceSortName
        , sentenceSortParameters
        , sentenceSortAttributes
        }
