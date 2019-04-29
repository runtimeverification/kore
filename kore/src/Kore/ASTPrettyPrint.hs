{-|
Module      : Kore.ASTPrettyPrint
Description : Pretty-printer for internal representations of Kore
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
-}

module Kore.ASTPrettyPrint
    ( prettyPrintToString
    , PrettyPrint(..)
    , Flags(..)
    ) where

import           Control.Comonad.Trans.Cofree
                 ( Cofree, CofreeT (..) )
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.List
                 ( intersperse )
import qualified Data.Set as Set
import           Data.String
                 ( fromString )
import           Data.Text.Prettyprint.Doc as Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Void
import           Numeric.Natural

import           Data.Sup
import qualified Kore.Annotation.Null as Annotation
import           Kore.AST.Pure
import           Kore.AST.Sentence
import           Kore.AST.Valid
import qualified Kore.Builtin.Error as Builtin
import qualified Kore.Domain.Builtin as Domain
import           Kore.Predicate.Predicate
import           Kore.Proof.Functional
import           Kore.Step.Conditional
import qualified Kore.Step.Pattern as Step
                 ( Pattern )
import           Kore.Syntax.StringLiteral
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.Unifier
import           Kore.Unparser
                 ( escapeCharT, escapeString, escapeStringT )

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}
{-
This module uses the following pattern repeatedly:
instance PrettyPrint Something where
    prettyPrint s@(Something _ _ _ ...) =
        writeStructure "Something"
            [ writeField "something1" something1 s
            , writeField "something2" something2 s
            ....
            ]

This pattern does a few things, which are very nice to have in a pretty printer:
1. Makes sure that we will notice when the number of fields in Something
   changes (done by matching on (Something _ _ _ ...)).
2. Makes sure that the code still works when the field order changes
   (done by using the something<n> accessors).
3. Makes sure that we notice when field names change (again, done by using
   the something<n> accessors).

However, this generates the HLint error disabled above.
-}

data Flags = NeedsParentheses | MaySkipParentheses

{-  Print to string instance
-}

class PrettyPrint a where
    prettyPrint :: Flags -> a -> Doc ann

prettyPrintToString :: PrettyPrint a => a -> String
prettyPrintToString a = renderString (layoutPretty options doc)
  where
    doc = prettyPrint MaySkipParentheses a
    options = defaultLayoutOptions { layoutPageWidth = Unbounded }

{- utility functions -}

betweenParentheses :: Flags -> Doc ann -> Doc ann
betweenParentheses NeedsParentheses thing = parens thing
betweenParentheses MaySkipParentheses thing = thing

writeOneFieldStruct
    :: (PrettyPrint a)
    => Flags -> String -> a -> Doc ann
writeOneFieldStruct flags name content =
    writeOneFieldStructK flags name (prettyPrint NeedsParentheses content)

writeTwoFieldStruct
    :: (PrettyPrint a, PrettyPrint b)
    => Flags -> String -> a -> b -> Doc ann
writeTwoFieldStruct flags name contenta contentb =
    writeOneFieldStructK
        flags
        name
        (   prettyPrint NeedsParentheses contenta
        <+> prettyPrint NeedsParentheses contentb
        )

writeThreeFieldStruct
    :: (PrettyPrint a, PrettyPrint b, PrettyPrint c)
    => Flags -> String -> a -> b -> c -> Doc ann
writeThreeFieldStruct flags name contenta contentb contentc =
    writeOneFieldStructK
        flags
        name
        (   prettyPrint NeedsParentheses contenta
        <+> prettyPrint NeedsParentheses contentb
        <+> prettyPrint NeedsParentheses contentc
        )

writeOneFieldStructK
    :: Flags -> String -> Doc ann -> Doc ann
writeOneFieldStructK flags name fieldWriterAction =
    betweenParentheses
        flags
        (fromString name <+> fieldWriterAction)

writeFieldOneLine
    :: (PrettyPrint a) => String -> (b -> a) -> b -> Doc ann
writeFieldOneLine fieldName field object =
    fromString fieldName
    <+> "="
    <+> prettyPrint MaySkipParentheses (field object)

writeListField
    :: (PrettyPrint a) => String -> (b -> a) -> b -> Doc ann
writeListField fieldName field object =
    fromString fieldName
    <+> "=" <> prettyPrint MaySkipParentheses (field object)

writeFieldNewLine
    :: (PrettyPrint a) => String -> (b -> a) -> b -> Doc ann
writeFieldNewLine fieldName field object =
    fromString fieldName
    <+> "="
    <> nest 4
        (Doc.line <> prettyPrint MaySkipParentheses (field object))

writeAttributesField
    :: String
    -> Attributes
    -> Doc ann
writeAttributesField fieldName attributes@(Attributes as) =
    fromString fieldName
    <+> "=" <>
    if null as
        then space <> prettyPrint MaySkipParentheses attributes
        else
            nest 4
                (Doc.line <> prettyPrint MaySkipParentheses attributes)

writeStructure :: String -> [Doc ann] -> Doc ann
writeStructure name fields =
    fromString name <> inCurlyBracesIndent (printableList fields)

printableList :: [Doc ann] -> [Doc ann]
printableList = intersperse (Doc.line <> comma <> space)

instance PrettyPrint Void where
    prettyPrint _ = \case {}

instance PrettyPrint () where
    prettyPrint _ () = "()"

instance PrettyPrint Natural where
    prettyPrint _  = pretty

instance PrettyPrint Id where
    prettyPrint flags (Id name _) =
        betweenParentheses flags
        $ Doc.sep [ "Id", (dquotes . pretty) name, "AstLocationNone" ]

instance PrettyPrint a => PrettyPrint [a] where
    prettyPrint _ items =
        inSquareBracketsIndent
            (printableList (map (prettyPrint MaySkipParentheses) items))

instance PrettyPrint a => PrettyPrint (Set.Set a) where
    prettyPrint flags = prettyPrint flags . Set.toList

listWithDelimiters :: String -> String -> [Doc ann] -> Doc ann
listWithDelimiters start end [] =
    " " <> fromString start <> fromString end
listWithDelimiters start end items =
    nest 4 $
        Doc.line
        <> fromString start
        <+> hcat items
        <> Doc.line
        <> fromString end

inCurlyBracesIndent :: [Doc ann] -> Doc ann
inCurlyBracesIndent = listWithDelimiters "{" "}"

inSquareBracketsIndent :: [Doc ann] -> Doc ann
inSquareBracketsIndent = listWithDelimiters "[" "]"

instance PrettyPrint SortVariable where
    prettyPrint flags sv =
        writeOneFieldStruct flags "SortVariable" (getSortVariable sv)

instance  PrettyPrint Sort where
    prettyPrint flags (SortVariableSort sv) =
        writeOneFieldStruct flags "SortVariableSort" sv
    prettyPrint flags (SortActualSort sa)   =
        writeOneFieldStruct flags "SortActualSort" sa

instance  PrettyPrint SortActual where
    prettyPrint _ sa@(SortActual _ _) =
        writeStructure "SortActual"
            [ writeFieldOneLine "sortActualName" sortActualName sa
            , writeListField "sortActualSorts" sortActualSorts sa
            ]

instance PrettyPrint StringLiteral where
    prettyPrint flags (StringLiteral s) =
        betweenParentheses
            flags
            ("StringLiteral "
            <> dquotes (pretty (escapeStringT s))
            )

instance PrettyPrint CharLiteral where
    prettyPrint flags (CharLiteral c) =
        betweenParentheses
            flags
            ("CharLiteral "
            <> squotes (pretty (escapeCharT c))
            )

instance  PrettyPrint (SymbolOrAlias level) where
    prettyPrint _ s@(SymbolOrAlias _ _) =
        writeStructure "SymbolOrAlias"
            [ writeFieldOneLine
                "symbolOrAliasConstructor" symbolOrAliasConstructor s
            , writeListField "symbolOrAliasParams" symbolOrAliasParams s
            ]

instance  PrettyPrint (Alias level) where
    prettyPrint _ s@(Alias _ _) =
        writeStructure "Alias"
            [ writeFieldOneLine "aliasConstructor" aliasConstructor s
            , writeListField "aliasParams" aliasParams s
            ]

instance  PrettyPrint (Symbol level) where
    prettyPrint _ s@(Symbol _ _) =
        writeStructure "Symbol"
            [ writeFieldOneLine "symbolConstructor" symbolConstructor s
            , writeListField "symbolParams" symbolParams s
            ]

instance PrettyPrint ModuleName where
    prettyPrint flags s@(ModuleName _) =
        betweenParentheses
            flags
            ( "ModuleName "
            <> dquotes (pretty (getModuleName s))
            )

instance  PrettyPrint Variable where
    prettyPrint _ var@(Variable _ _ _) =
        writeStructure "Variable"
            [ writeFieldOneLine "variableName" variableName var
            , writeFieldOneLine "variableCounter" variableCounter var
            , writeFieldNewLine "variableSort" variableSort var
            ]

instance PrettyPrint variable
    => PrettyPrint (SetVariable variable)
  where
    prettyPrint _ svar@(SetVariable _) =
        writeStructure "SetVariable"
            [writeFieldNewLine "getVariable" getVariable svar]

instance PrettyPrint child => PrettyPrint (And level child) where
    prettyPrint _ p@(And _ _ _) =
        writeStructure
            "And"
            [ writeFieldNewLine "andSort" andSort p
            , writeFieldNewLine "andFirst" andFirst p
            , writeFieldNewLine "andSecond" andSecond p
            ]

instance PrettyPrint child => PrettyPrint (Application level child) where
    prettyPrint _ p@(Application _ _) =
        writeStructure
            "Application"
            [ writeFieldNewLine
                "applicationSymbolOrAlias" applicationSymbolOrAlias p
            , writeListField "applicationChildren" applicationChildren p
            ]

instance PrettyPrint (Bottom level child) where
    prettyPrint flags (Bottom p) =
        writeOneFieldStruct flags "Bottom" p

instance PrettyPrint child => PrettyPrint (Ceil level child) where
    prettyPrint _ p@(Ceil _ _ _) =
        writeStructure
            "Ceil"
            [ writeFieldNewLine "ceilOperandSort" ceilOperandSort p
            , writeFieldNewLine "ceilResultSort" ceilResultSort p
            , writeFieldNewLine "ceilChild" ceilChild p
            ]

instance PrettyPrint a => PrettyPrint (Const a b) where
    prettyPrint flags (Const a) = writeOneFieldStruct flags "Const" a

instance PrettyPrint (Annotation.Null level) where
    prettyPrint _ Annotation.Null = "Null"

instance PrettyPrint variable => PrettyPrint (Valid variable level) where
    prettyPrint _ valid@(Valid _ _) =
        writeStructure
            "Valid"
            [ writeFieldOneLine "patternSort" patternSort valid
            , writeFieldNewLine
                "freeVariables"
                Kore.AST.Valid.freeVariables
                valid
            ]

instance PrettyPrint child => PrettyPrint (Domain.Builtin child) where
    prettyPrint flags =
        \case
            Domain.BuiltinExternal str ->
                betweenParentheses flags
                $ "Domain.BuiltinExternal " <> prettyPrint NeedsParentheses str
            _ -> Builtin.notImplementedInternal

instance PrettyPrint child => PrettyPrint (Domain.External child) where
    prettyPrint _ p =
        writeStructure
            "Domain.External"
            [ writeFieldNewLine "domainValueSort"  Domain.domainValueSort  p
            , writeFieldNewLine "domainValueChild" Domain.domainValueChild p
            ]

instance
    ( PrettyPrint child
    , PrettyPrint (domain child)
    ) => PrettyPrint (DomainValue level domain child) where
    prettyPrint _ p@(DomainValue _ _) =
        writeStructure
            "DomainValue"
            [ writeFieldNewLine "domainValueSort" domainValueSort p
            , writeFieldNewLine "domainValueChild" domainValueChild p
            ]

instance PrettyPrint child => PrettyPrint (Equals level child) where
    prettyPrint _ p@(Equals _ _ _ _) =
        writeStructure
            "Equals"
            [ writeFieldNewLine "equalsOperandSort" equalsOperandSort p
            , writeFieldNewLine "equalsResultSort" equalsResultSort p
            , writeFieldNewLine "equalsFirst" equalsFirst p
            , writeFieldNewLine "equalsSecond" equalsSecond p
            ]

instance
    ( PrettyPrint child
    , PrettyPrint variable
    ) => PrettyPrint (Exists level variable child)
  where
    prettyPrint _ p@(Exists _ _ _) =
        writeStructure
            "Exists"
            [ writeFieldNewLine "existsSort" existsSort p
            , writeFieldNewLine "existsVariable" existsVariable p
            , writeFieldNewLine "existsChild" existsChild p
            ]

instance PrettyPrint child => PrettyPrint (Floor level child) where
    prettyPrint _ p@(Floor _ _ _) =
        writeStructure
            "Floor"
            [ writeFieldNewLine "floorOperandSort" floorOperandSort p
            , writeFieldNewLine "floorResultSort" floorResultSort p
            , writeFieldNewLine "floorChild" floorChild p
            ]

instance
    ( PrettyPrint child
    , PrettyPrint variable
    ) => PrettyPrint (Forall level variable child) where
    prettyPrint _ p@(Forall _ _ _) =
        writeStructure
            "Forall"
            [ writeFieldNewLine "forallSort" forallSort p
            , writeFieldNewLine "forallVariable" forallVariable p
            , writeFieldNewLine "forallChild" forallChild p
            ]

instance PrettyPrint child => PrettyPrint (Iff level child) where
    prettyPrint _ p@(Iff _ _ _) =
        writeStructure
            "Iff"
            [ writeFieldNewLine "iffSort" iffSort p
            , writeFieldNewLine "iffFirst" iffFirst p
            , writeFieldNewLine "iffSecond" iffSecond p
            ]

instance PrettyPrint child => PrettyPrint (Implies level child) where
    prettyPrint _ p@(Implies _ _ _) =
        writeStructure
            "Implies"
            [ writeFieldNewLine "impliesSort" impliesSort p
            , writeFieldNewLine "impliesFirst" impliesFirst p
            , writeFieldNewLine "impliesSecond" impliesSecond p
            ]

instance PrettyPrint child => PrettyPrint (In level child) where
    prettyPrint _ p@(In _ _ _ _) =
        writeStructure
            "In"
            [ writeFieldNewLine "inOperandSort" inOperandSort p
            , writeFieldNewLine "inResultSort" inResultSort p
            , writeFieldNewLine "inContainedChild" inContainedChild p
            , writeFieldNewLine "inContainingChild" inContainingChild p
            ]

instance PrettyPrint child => PrettyPrint (Next level child) where
    prettyPrint _ p@(Next _ _) =
        writeStructure
            "Next"
            [ writeFieldNewLine "nextSort" nextSort p
            , writeFieldNewLine "nextChild" nextChild p
            ]

instance PrettyPrint child => PrettyPrint (Not level child) where
    prettyPrint _ p@(Not _ _) =
        writeStructure
            "Not"
            [ writeFieldNewLine "notSort" notSort p
            , writeFieldNewLine "notChild" notChild p
            ]

instance PrettyPrint child => PrettyPrint (Or level child) where
    prettyPrint _ p@(Or _ _ _) =
        writeStructure
            "Or"
            [ writeFieldNewLine "orSort" orSort p
            , writeFieldNewLine "orFirst" orFirst p
            , writeFieldNewLine "orSecond" orSecond p
            ]

instance PrettyPrint child => PrettyPrint (Rewrites level child) where
    prettyPrint _ p@(Rewrites _ _ _) =
        writeStructure
            "Rewrites"
            [ writeFieldNewLine "rewritesSort" rewritesSort p
            , writeFieldNewLine "rewritesFirst" rewritesFirst p
            , writeFieldNewLine "rewritesSecond" rewritesSecond p
            ]

instance  PrettyPrint (Top level child) where
    prettyPrint flags (Top p) =
        writeOneFieldStruct flags "Top" p

instance
    ( PrettyPrint child
    , PrettyPrint (domain child)
    , PrettyPrint variable
    ) => PrettyPrint (Pattern level domain variable child)
  where
    prettyPrint flags (AndPattern p) =
        writeOneFieldStruct flags "AndPattern" p
    prettyPrint flags (ApplicationPattern p)   =
        writeOneFieldStruct flags "ApplicationPattern" p
    prettyPrint flags (BottomPattern p)        =
        writeOneFieldStruct flags "BottomPattern" p
    prettyPrint flags (CeilPattern p)          =
        writeOneFieldStruct flags "CeilPattern" p
    prettyPrint flags (DomainValuePattern p)          =
        writeOneFieldStruct flags "DomainValuePattern" p
    prettyPrint flags (EqualsPattern p)        =
        writeOneFieldStruct flags "EqualsPattern" p
    prettyPrint flags (ExistsPattern p)        =
        writeOneFieldStruct flags "ExistsPattern" p
    prettyPrint flags (FloorPattern p)         =
        writeOneFieldStruct flags "FloorPattern" p
    prettyPrint flags (ForallPattern p)        =
        writeOneFieldStruct flags "ForallPattern" p
    prettyPrint flags (IffPattern p)           =
        writeOneFieldStruct flags "IffPattern" p
    prettyPrint flags (ImpliesPattern p)       =
        writeOneFieldStruct flags "ImpliesPattern" p
    prettyPrint flags (InPattern p)            =
        writeOneFieldStruct flags "InPattern" p
    prettyPrint flags (NextPattern p)          =
        writeOneFieldStruct flags "NextPattern" p
    prettyPrint flags (NotPattern p)           =
        writeOneFieldStruct flags "NotPattern" p
    prettyPrint flags (OrPattern p)            =
        writeOneFieldStruct flags "OrPattern" p
    prettyPrint flags (RewritesPattern p)      =
        writeOneFieldStruct flags "RewritesPattern" p
    prettyPrint flags (StringLiteralPattern p) =
        writeOneFieldStruct flags "StringLiteralPattern" p
    prettyPrint flags (CharLiteralPattern p) =
        writeOneFieldStruct flags "CharLiteralPattern" p
    prettyPrint flags (TopPattern p)           =
        writeOneFieldStruct flags "TopPattern" p
    prettyPrint flags (VariablePattern p)      =
        writeOneFieldStruct flags "VariablePattern" p
    prettyPrint flags (InhabitantPattern s)          =
        writeOneFieldStruct flags "InhabitantPattern" s
    prettyPrint flags (SetVariablePattern p)      =
        writeOneFieldStruct flags "SetVariablePattern" p

instance
    ( self ~ CofreeT f w a
    , PrettyPrint (w (CofreeF f a self))
    ) =>
    PrettyPrint (CofreeT f w a)
  where
    prettyPrint _ cofreet =
        writeStructure "CofreeT"
            [ writeFieldOneLine "runCofreeT" runCofreeT cofreet ]

instance (PrettyPrint a, PrettyPrint (f b)) => PrettyPrint (CofreeF f a b) where
    prettyPrint flags (a :< fb) =
        betweenParentheses flags
        (   prettyPrint MaySkipParentheses a
        <+> ":<"
        <+> prettyPrint MaySkipParentheses fb
        )

instance PrettyPrint a => PrettyPrint (Identity a) where
    prettyPrint _ identity =
        writeStructure "Identity"
            [ writeFieldOneLine "runIdentity" runIdentity identity ]

instance
    ( Functor domain
    , PrettyPrint child
    , PrettyPrint annotation
    , PrettyPrint (domain child)
    , child ~ Cofree (Pattern level domain variable) annotation
    ) =>
    PrettyPrint (PurePattern level domain variable annotation)
  where
    prettyPrint _ purePattern =
        writeStructure "PurePattern"
            [ writeFieldOneLine "getPurePattern" getPurePattern purePattern ]

instance PrettyPrint Attributes
  where
    prettyPrint flags (Attributes a)
        | null a    = "Attributes []"
        | otherwise = writeOneFieldStruct flags "Attributes" a

instance
    PrettyPrint patternType
    => PrettyPrint (SentenceAlias level patternType)
  where
    prettyPrint _ sa@(SentenceAlias _ _ _ _ _ _) =
        writeStructure
            "SentenceAlias"
            [ writeFieldNewLine "sentenceAliasAlias" sentenceAliasAlias sa
            , writeListField "sentenceAliasSorts" sentenceAliasSorts sa
            , writeFieldNewLine
                "sentenceAliasReturnSort"
                sentenceAliasResultSort
                sa
            , writeFieldNewLine
                "sentenceAliasLeftPattern"
                sentenceAliasLeftPattern
                sa
            , writeFieldNewLine
                "sentenceAliasRightPattern"
                sentenceAliasRightPattern
                sa
            , writeAttributesField
                "sentenceAliasAttributes" (sentenceAliasAttributes sa)
            ]

instance
    PrettyPrint patternType
    => PrettyPrint (SentenceSymbol level patternType)
  where
    prettyPrint _ sa@(SentenceSymbol _ _ _ _) =
        writeStructure
            "SentenceSymbol"
            [ writeFieldNewLine "sentenceSymbolSymbol" sentenceSymbolSymbol sa
            , writeListField "sentenceSymbolSorts" sentenceSymbolSorts sa
            , writeFieldNewLine
                "sentenceSymbolReturnSort" sentenceSymbolResultSort sa
            , writeAttributesField
                "sentenceSymbolAttributes" (sentenceSymbolAttributes sa)
            ]

instance
    (PrettyPrint patternType) => PrettyPrint (SentenceImport patternType)
  where
    prettyPrint _ sa@(SentenceImport _ _) =
        writeStructure
            "SentenceImport"
            [ writeFieldNewLine
                "sentenceImportModuleName" sentenceImportModuleName sa
            , writeAttributesField
                "sentenceAxiomAttributes" (sentenceImportAttributes sa)
            ]

instance
    ( PrettyPrint sortParam
    , PrettyPrint patternType
    ) => PrettyPrint (SentenceAxiom sortParam patternType)
  where
    prettyPrint _ sa@(SentenceAxiom _ _ _) =
        writeStructure
            "SentenceAxiom"
            [ writeListField
                "sentenceAxiomParameters" sentenceAxiomParameters sa
            , writeFieldNewLine
                "sentenceAxiomPattern" sentenceAxiomPattern sa
            , writeAttributesField
                "sentenceAxiomAttributes" (sentenceAxiomAttributes sa)
            ]

instance
    PrettyPrint patternType
    => PrettyPrint (SentenceSort level patternType)
  where
    prettyPrint _ sa@(SentenceSort _ _ _) =
        writeStructure
            "SentenceSort"
            [ writeFieldOneLine "sentenceSortName" sentenceSortName sa
            , writeListField
                "sentenceSortParameters" sentenceSortParameters sa
            , writeAttributesField
                "sentenceSortAttributes" (sentenceSortAttributes sa)
            ]

instance PrettyPrint patternType => PrettyPrint (SentenceHook patternType) where
    prettyPrint flags (SentenceHookedSymbol s)   =
        writeOneFieldStruct flags "SentenceHookedSymbol" s
    prettyPrint flags (SentenceHookedSort s)         =
        writeOneFieldStruct flags "SentenceHookedSort" s

instance
    ( PrettyPrint sortParam
    , PrettyPrint patternType
    ) => PrettyPrint (Sentence level sortParam patternType)
  where
    prettyPrint flags (SentenceAliasSentence s)    =
        writeOneFieldStruct flags "SentenceAliasSentence" s
    prettyPrint flags (SentenceSymbolSentence s)   =
        writeOneFieldStruct flags "SentenceSymbolSentence" s
    prettyPrint flags (SentenceImportSentence s)        =
        writeOneFieldStruct flags "SentenceImportSentence" s
    prettyPrint flags (SentenceAxiomSentence s)        =
        writeOneFieldStruct flags "SentenceAxiomSentence" s
    prettyPrint flags (SentenceClaimSentence s)        =
        writeOneFieldStruct flags "SentenceClaimSentence" s
    prettyPrint flags (SentenceSortSentence s)         =
        writeOneFieldStruct flags "SentenceSortSentence" s
    prettyPrint flags (SentenceHookSentence s)         =
        writeOneFieldStruct flags "SentenceHookSentence" s

instance PrettyPrint sentence => PrettyPrint (Module sentence) where
    prettyPrint _ m@(Module _ _ _) =
        writeStructure
            "Module"
            [ writeFieldOneLine "moduleName" moduleName m
            , writeListField "moduleSentences" moduleSentences m
            , writeAttributesField "moduleAttributes" (moduleAttributes m)
            ]

instance PrettyPrint sentence => PrettyPrint (Definition sentence) where
    prettyPrint _ d@(Definition _ _) =
        writeStructure
            "Definition"
            [ writeAttributesField
                "definitionAttributes" (definitionAttributes d)
            , writeListField "definitionModules" definitionModules d
            ]

instance PrettyPrint a => PrettyPrint (Maybe a) where
    prettyPrint flags (Just x) =
        writeOneFieldStruct flags "Just" x
    prettyPrint _ Nothing = "Nothing"

instance PrettyPrint a => PrettyPrint (Sup a) where
    prettyPrint flags (Element x) =
        writeOneFieldStruct flags "Element" x
    prettyPrint _ Sup = "Sup"

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
    prettyPrint flags (Left x) =
        writeOneFieldStruct flags "Left" x
    prettyPrint flags (Right x) =
        writeOneFieldStruct flags "Right" x

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
    prettyPrint _ (x, y) =
        listWithDelimiters "(" ")"
            (printableList
                [ prettyPrint MaySkipParentheses x
                , prettyPrint MaySkipParentheses y
                ])

-- TODO: when refactoring these, consider removing `writeTwoFieldStruct`
-- TODO: when refactoring these, consider removing `writeThreeFieldStruct`
instance PrettyPrint variable => PrettyPrint (UnificationProof level variable)
  where
    prettyPrint _ EmptyUnificationProof = "EmptyUnificationProof"
    prettyPrint flags (CombinedUnificationProof p) =
        writeOneFieldStruct flags "CombinedUnificationProof" p
    prettyPrint flags (ConjunctionIdempotency p) =
        writeOneFieldStruct flags "ConjunctionIdempotency" p
    prettyPrint flags (AndDistributionAndConstraintLifting patternHead proofs) =
        writeTwoFieldStruct
            flags
            "AndDistributionAndConstraintLifting"
            patternHead
            proofs
    prettyPrint flags (Proposition_5_24_3 funProof var pat) =
        writeThreeFieldStruct flags "Proposition_5_24_3" funProof var pat
    prettyPrint flags (SubstitutionMerge var pat1 pat2) =
        writeThreeFieldStruct flags "SubstitutionMerge" var pat1 pat2

-- TODO: when refactoring these, consider removing `writeTwoFieldStruct`
instance  PrettyPrint (ClashReason level) where
    prettyPrint flags (DomainValueClash h) =
        betweenParentheses
            flags
            ("DomainValueClash "
            <> dquotes (fromString (escapeString h))
            )
    prettyPrint flags (HeadClash h) =
        writeOneFieldStruct flags "HeadClash" h
    prettyPrint flags (SortInjectionClash s1 s2) =
        writeTwoFieldStruct flags "SortInjectionClash" s1 s2

instance PrettyPrint variable => PrettyPrint (FunctionalProof level variable)
  where
    prettyPrint flags (FunctionalVariable v) =
        writeOneFieldStruct flags "FunctionalVariable" v
    prettyPrint flags (FunctionalDomainValue dv) =
        writeOneFieldStruct flags "FunctionalDomainValue" dv
    prettyPrint flags (FunctionalHead h) =
        writeOneFieldStruct flags "FunctionalHead" h
    prettyPrint flags (FunctionalStringLiteral l) =
        writeOneFieldStruct flags "FunctionalStringLiteral" l
    prettyPrint flags (FunctionalCharLiteral l) =
        writeOneFieldStruct flags "FunctionalCharLiteral" l

instance PrettyPrint variable => PrettyPrint (Predicate variable) where
    prettyPrint flags pat =
        prettyPrint flags (unwrapPredicate pat)

instance PrettyPrint variable => PrettyPrint (Substitution variable) where
      prettyPrint flags = prettyPrint flags . Substitution.unwrap

instance PrettyPrint variable => PrettyPrint (Step.Pattern level variable) where
    prettyPrint flags (Conditional t p s) =
        writeThreeFieldStruct flags "Conditional" t p s
