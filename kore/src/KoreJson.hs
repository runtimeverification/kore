{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

{-# LANGUAGE OverloadedStrings #-}

{-# Options -Wno-partial-fields #-}
{-# Options -Wno-duplicate-exports #-}
-- while exporting the entire module

module KoreJson (
    -- API
   JsonError (..),
   encodePattern,
   decodePattern,
    -- export everything for debugging and testing only
    module KoreJson,
) where

import Data.Aeson as Json
import Data.Aeson.Types as Json
import Data.Aeson.Encode.Pretty as Json
import Data.ByteString.Lazy (ByteString)
import Data.Char (isAlpha, isDigit)
import Data.Either.Extra hiding (Left, Right)
import Data.Functor.Const (Const (..))
import Data.Functor.Foldable as Recursive (Recursive (..))
import Data.List (foldl1')
import Data.Sup (Sup (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics -- FIXME switch to TH-generated Json instances
import Kore.Attribute.Attributes (ParsedPattern)
import Kore.Parser (embedParsedPattern)
import Kore.Syntax qualified as Kore
import Kore.Syntax.PatternF (PatternF (..))
import Kore.Syntax.Variable (
    ElementVariableName (..),
    SetVariableName (..),
    SomeVariableName (..),
    Variable (..),
    VariableName (..),
 )
import Prelude.Kore hiding (Left, Right)
import Prelude.Kore qualified as Prelude (Either (..))

{- | Json representation of Kore patterns as a Haskell type.
 Modeled after kore-syntax.md, merging some of the ML pattern
 productions. Cursorily checked to be consistent with Parser.y
-}
data KorePattern
    = -- variable pattern

      -- | element variable with sort
      KJEVar
        { name :: Id
        , sort :: Sort
        }
    | -- | set variable with sort
      KJSVar
        { name :: Id -- must start by '@'
        , sort :: Sort
        }
    | -- application pattern

      -- | application pattern (symbol, [arg] )
      KJApp
        { name :: Id -- may start by a '\\'
        , sorts :: [Sort]
        , args :: [KorePattern]
        }
    | -- | string literal
      KJString Text
    | -- matching logic pattern

      -- | Connective (top, bottom, not, and, or, implies, iff)
      KJTop
        { sort :: Sort
        }
    | KJBottom
        { sort :: Sort
        }
    | KJNot
        { sort :: Sort
        , arg :: KorePattern
        }
    | KJAnd
        { sort :: Sort
        , first :: KorePattern
        , second :: KorePattern
        }
    | KJOr
        { sort :: Sort
        , first :: KorePattern
        , second :: KorePattern
        }
    | KJImplies
        { sort :: Sort
        , first :: KorePattern
        , second :: KorePattern
        }
    | KJIff
        { sort :: Sort
        , first :: KorePattern
        , second :: KorePattern
        }

    | -- | Quantifiers: forall, exists
      KJForall
        { sort :: Sort
        , var :: Id
        , varSort :: Sort
        , arg :: KorePattern
        }
    | KJExists
        { sort :: Sort
        , var :: Id
        , varSort :: Sort
        , arg :: KorePattern
        }
    | -- | mu, nu
      KJMu
        { var :: Id
        , varSort :: Sort
        , arg :: KorePattern
        }
    | KJNu
        { var :: Id
        , varSort :: Sort
        , arg :: KorePattern
        }
    | -- | ceil, floor, equals, in
      KJCeil
        { argSort :: Sort
        , resultSort :: Sort
        , arg :: KorePattern
        }
    | KJFloor
        { argSort :: Sort
        , resultSort :: Sort
        , arg :: KorePattern
        }
    | KJEquals
        { argSort :: Sort
        , resultSort :: Sort
        , first :: KorePattern
        , second :: KorePattern
        }
    | KJIn
        { argSort :: Sort
        , resultSort :: Sort
        , first :: KorePattern
        , second :: KorePattern
        }
    | -- next, rewrites
      -- | goes to 'dest' next
      KJNext
        { sort :: Sort
        , dest :: KorePattern
        }
    | -- | source rewrites to dest (same sort)
      KJRewrites
        { sort :: Sort
        , source :: KorePattern
        , dest :: KorePattern
        }
    | -- | domain value, a string literal with a sort
      KJDv
        { sort :: Sort
        , value :: Text
        }
    | -- syntactic sugar

      -- | left/right associative or-cascade
      KJMultiOr
        { assoc :: LeftRight
        , sort :: Sort
        , args :: [KorePattern]
        }
    | -- TODO textual parser also understands And/Implies/Iff

      -- | left/right associative app pattern
      KJMultiApp
        { assoc :: LeftRight
        , symbol :: Id -- may start by a '\\'
        , sorts :: [Sort]
        , args :: [KorePattern]
        }
    deriving stock (Eq, Show, Generic)

instance ToJSON KorePattern where
    toJSON = genericToJSON codecOptions

instance FromJSON KorePattern where
    parseJSON v = genericParseJSON codecOptions v >>= lexicalCheck

codecOptions :: Json.Options
codecOptions =
    Json.defaultOptions
        { constructorTagModifier
        , omitNothingFields = True
        , sumEncoding = TaggedObject "tag" "contents"
        , unwrapUnaryRecords = True
        , tagSingleConstructors = True
        , rejectUnknownFields = True
        }
  where
    constructorTagModifier = \case
        "KJDv" -> "dv"
        'K':'J':rest -> rest
        other -> other

----------------------------------------
-- Identifiers and lexical checks

-- | Performs a (shallow, top-level, no recursion) lexical check of
-- identifiers contained in the given node.
--
-- Basic identifiers start with letters and may contain letters,
-- digits, _ or '. Set variables start with '@' followed by a basic
-- identifier. Symbol variables _may_ start by \, followed by a basic
-- identifier.
--
-- String literals may contain printable Ascii characters (0x20 -
-- 0x7e) except " and \, escape sequences \t, \n, \f, \r, \", \\, or
-- Unicode escape sequences with 1, 2 or 4 bytes: \sHH, \uHHHH, \UHHHHHH
lexicalCheck :: KorePattern -> Parser KorePattern
lexicalCheck x = pure x -- TODO

newtype Id = Id Text
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

checkIdChars :: Text -> [ String ] -- list of lexical errors
-- FIXME collect all lexical errors instead of stopping at one
checkIdChars name
    | T.null name = [ "Empty" ]
checkIdChars name
    | not $ isAlpha first =
          [ "illegal initial character" ]
    | not $ T.all isIdChar rest =
          [ "contains illegal characters" ]
    | otherwise = []
  where
    first = T.head name
    rest = T.tail name

isIdChar :: Char -> Bool
isIdChar  c = isAlpha c || isDigit c || c `elem` ['_', '\'' ]

-- | has to start with `@`, followed by a valid identifier
checkSVarName :: Text -> [ String ]
checkSVarName name
    | T.null name = [ "empty" ]
checkSVarName name
    | T.head name /= '@' = [ "must start with `@'" ]
    | otherwise = checkIdChars (T.tail name)

data Sort
    = Sort
        { name :: Id -- may start by a backslash
        , args :: [Sort]
        }
    | SortVariable Text -- may start by a backslash
    deriving stock (Eq, Show, Generic)

instance ToJSON Sort where
    toJSON = genericToJSON codecOptions

instance FromJSON Sort where
    parseJSON = genericParseJSON codecOptions

data LeftRight
    = Left
    | Right
    deriving stock (Eq, Show, Generic, Enum, Bounded)
    deriving anyclass (ToJSON, FromJSON)

------------------------------------------------------------
-- reading

{- | read text into KorePattern, then check for consistency and
 construct a ParsedPattern
-}
decodePattern :: ByteString -> Either JsonError ParsedPattern
decodePattern bs =
    mapLeft ParseError (decodeKoreJson bs) >>= toParsedPattern

-- | Errors relating to the json codec
data JsonError
    = -- | Problem reported by json parser
      ParseError String
    | -- | Wrong arg count for a connective or other construct
      WrongArgCount String Int
    | -- | MultiOr/MultiApp require a non-empty argument list
      MissingArg String
    deriving stock (Eq, Show)

-- | low-level: read text into KorePattern
decodeKoreJson :: ByteString -> Either String KorePattern
decodeKoreJson = Json.eitherDecode'

-- see Parser.y
toParsedPattern :: KorePattern -> Either JsonError ParsedPattern
toParsedPattern = \case
    KJEVar n s ->
        fmap (embedParsedPattern . VariableF . Const) $
            embedVar (SomeVariableNameElement . ElementVariableName) n s
    KJSVar n s ->
        fmap (embedParsedPattern . VariableF . Const) $
            embedVar (SomeVariableNameSet . SetVariableName) n s
    KJApp n ss as ->
        fmap (embedParsedPattern . ApplicationF) $
            Kore.Application <$> toSymbol n ss <*> traverse toParsedPattern as
    KJString t ->
        pure . embedParsedPattern . StringLiteralF . Const $ Kore.StringLiteral t
    KJTop s ->
        fmap (embedParsedPattern . TopF) $
            Kore.Top <$> (mkSort s)
    KJBottom s ->
        fmap (embedParsedPattern . BottomF) $
            Kore.Bottom <$> (mkSort s)
    KJNot s a ->
        fmap (embedParsedPattern . NotF) $
            Kore.Not <$> mkSort s <*> toParsedPattern a
    KJAnd s a b ->
        fmap (embedParsedPattern . AndF) $
            Kore.And <$> mkSort s <*> toParsedPattern a <*> toParsedPattern b
    KJOr s a b ->
        fmap (embedParsedPattern . OrF) $
            Kore.Or <$> mkSort s <*> toParsedPattern a <*> toParsedPattern b
    KJImplies s a b ->
        fmap (embedParsedPattern . ImpliesF) $
            Kore.Implies <$> mkSort s <*> toParsedPattern a <*> toParsedPattern b
    KJIff s a b ->
        fmap (embedParsedPattern . IffF) $
            Kore.Iff <$> mkSort s <*> toParsedPattern a <*> toParsedPattern b
    KJForall{sort, var, varSort, arg} ->
        fmap (embedParsedPattern . ForallF) $
            Kore.Forall
                <$> mkSort sort
                <*> (Variable (ElementVariableName (koreVar var)) <$> mkSort varSort)
                <*> toParsedPattern arg
    KJExists{sort, var, varSort, arg} ->
        fmap (embedParsedPattern . ExistsF) $
            Kore.Exists
                <$> mkSort sort
                <*> (Variable (ElementVariableName (koreVar var)) <$> mkSort varSort)
                <*> toParsedPattern arg
    KJMu{var, varSort, arg} ->
        fmap (embedParsedPattern . MuF) $
            Kore.Mu
                <$> (Variable (SetVariableName (koreVar var)) <$> mkSort varSort)
                <*> toParsedPattern arg
    KJNu{var, varSort, arg} ->
        fmap (embedParsedPattern . NuF) $
            Kore.Nu
                <$> (Variable (SetVariableName (koreVar var)) <$> mkSort varSort)
                <*> toParsedPattern arg
    KJCeil s s2 a ->
        fmap (embedParsedPattern .CeilF) $
            Kore.Ceil <$> mkSort s <*> mkSort s2 <*> toParsedPattern a
    KJFloor s s2 a ->
        fmap (embedParsedPattern .FloorF) $
            Kore.Floor <$> mkSort s <*> mkSort s2 <*> toParsedPattern a
    KJEquals s s2 a b ->
        fmap (embedParsedPattern .EqualsF) $
            Kore.Equals <$> mkSort s <*> mkSort s2 <*> toParsedPattern a <*> toParsedPattern b
    KJIn s s2 a b ->
        fmap (embedParsedPattern .InF) $
            Kore.In <$> mkSort s <*> mkSort s2 <*> toParsedPattern a <*> toParsedPattern b
    KJNext{sort, dest} ->
        fmap (embedParsedPattern . NextF) $
            Kore.Next
                <$> mkSort sort
                <*> toParsedPattern dest
    KJRewrites{sort, source, dest} ->
        fmap (embedParsedPattern . RewritesF) $
            Kore.Rewrites
                <$> mkSort sort
                <*> toParsedPattern source
                <*> toParsedPattern dest
    KJDv{sort, value} ->
        fmap (embedParsedPattern . DomainValueF) $
            Kore.DomainValue
                <$> mkSort sort
                <*> toParsedPattern (KJString value)
    KJMultiOr{assoc, sort, args}
        | null args -> Prelude.Left $ MissingArg "MultiOr"
        | otherwise ->
            withAssoc assoc <$> mkOr sort <*> traverse toParsedPattern args
    KJMultiApp{assoc, symbol, sorts, args}
        | null args -> Prelude.Left $ MissingArg "MultiApp"
        | otherwise ->
            withAssoc assoc <$> mkF symbol sorts <*> traverse toParsedPattern args
  where
    embedVar ::
        (VariableName -> SomeVariableName VariableName) ->
        Id ->
        Sort ->
        Either JsonError (Variable (SomeVariableName VariableName))
    embedVar cons n s =
        Variable <$> mkVarName cons n <*> mkSort s

    mkVarName ::
        (VariableName -> SomeVariableName VariableName) ->
        Id ->
        Either JsonError (SomeVariableName VariableName)
    mkVarName embed = pure . embed . koreVar

    toSymbol :: Id -> [Sort] -> Either JsonError Kore.SymbolOrAlias
    toSymbol n sorts = Kore.SymbolOrAlias (koreId n) <$> traverse mkSort sorts

    withAssoc :: LeftRight -> (a -> a -> a) -> [a] -> a
    withAssoc Left = foldl1'
    withAssoc Right = foldr1

    mkOr :: Sort -> Either JsonError (ParsedPattern -> ParsedPattern -> ParsedPattern)
    mkOr s = do
        sort <- mkSort s
        pure (\a b -> embedParsedPattern $ OrF $ Kore.Or sort a b)

    mkF :: Id -> [Sort] -> Either JsonError (ParsedPattern -> ParsedPattern -> ParsedPattern)
    mkF n sorts = do
        sym <- toSymbol n sorts -- TODO should maybe check that length ss == 2?
        pure (\a b -> embedParsedPattern $ ApplicationF $ Kore.Application sym [a, b])

koreId :: Id -> Kore.Id
koreId (Id name) = Kore.Id name Kore.AstLocationNone

koreVar :: Id -> Kore.VariableName
koreVar (Id name) =
    -- TODO check well-formed (initial letter, char. set)
    VariableName (Kore.Id base Kore.AstLocationNone) suffix
  where
    baseName = T.dropWhileEnd isDigit name
    endDigits = T.takeWhileEnd isDigit name
    (zeros, actualNum) = T.break (/= '0') endDigits
    (base, suffix)
        | T.null endDigits = (baseName, Nothing)
        | T.null actualNum = (baseName <> T.init zeros, Just $ Element 0)
        | otherwise =
            (baseName <> zeros, Just $ Element (read $ T.unpack actualNum))

mkSort :: Sort -> Either JsonError Kore.Sort
mkSort Sort{name, args} =
    fmap (Kore.SortActualSort . Kore.SortActual (koreId name)) $
        mapM mkSort args
mkSort (SortVariable name) =
    pure . Kore.SortVariableSort $ Kore.SortVariable (koreId $ Id name)

------------------------------------------------------------
-- writing

-- | Write a Pattern to a json byte string.
encodePattern :: Kore.Pattern VariableName ann -> ByteString
encodePattern = encodeKoreJson . fromPattern

encodeKoreJson :: KorePattern -> ByteString
encodeKoreJson = Json.encodePretty' prettyJsonOpts

prettyJsonOpts :: Json.Config
prettyJsonOpts =
    defConfig
        { confIndent = Spaces 2
        , confCompare =
            keyOrder -- retains the field order in all constructors
                [ "tag"
                , "assoc"
                , "name"
                , "symbol"
                , "sort"
                , "sorts"
                , "var"
                , "varSort"
                , "argSort"
                , "resultSort"
                , "arg"
                , "args"
                , "source"
                , "dest"
                , "value"
                ]
        }

fromPattern :: Kore.Pattern VariableName ann -> KorePattern
fromPattern pat =
    -- forget the annotation and recurse over the term-like PatternF
    let _ :< patF = Recursive.project pat
     in fromPatternF patF

fromPatternF :: Kore.PatternF VariableName (Kore.Pattern VariableName ann) -> KorePattern
fromPatternF = \case
    AndF Kore.And{andSort, andFirst, andSecond} ->
        KJAnd
            { sort = fromSort andSort
            , first = fromPattern andFirst
            , second = fromPattern andSecond
            }
    ApplicationF
        ( Kore.Application
                Kore.SymbolOrAlias{symbolOrAliasConstructor, symbolOrAliasParams}
                args
            ) ->
            KJApp
                { name = fromKoreId symbolOrAliasConstructor
                , sorts = map fromSort symbolOrAliasParams
                , args = map fromPattern args
                }
    BottomF Kore.Bottom{bottomSort} ->
        KJBottom { sort = fromSort bottomSort }
    CeilF Kore.Ceil{ceilOperandSort, ceilResultSort, ceilChild} ->
        KJCeil
            { argSort = fromSort ceilOperandSort
            , resultSort = fromSort ceilResultSort
            , arg = fromPattern ceilChild
            }
    DomainValueF Kore.DomainValue{domainValueSort, domainValueChild}
        | _ :< StringLiteralF (Const Kore.StringLiteral{getStringLiteral}) <-
            -- expected to contain a string literal value
            Recursive.project domainValueChild ->
            KJDv
                { sort = fromSort domainValueSort
                , value = getStringLiteral
                }
        | otherwise -> error "Bad domain value"
    EqualsF Kore.Equals{equalsOperandSort, equalsResultSort, equalsFirst, equalsSecond} ->
        KJEquals
            { argSort = fromSort equalsOperandSort
            , resultSort = fromSort equalsResultSort
            , first = fromPattern equalsFirst
            , second = fromPattern equalsSecond
            }
    ExistsF Kore.Exists{existsSort, existsVariable, existsChild} ->
        KJExists
            { sort = fromSort existsSort
            , var = fromKoreVariableName $ Kore.unElementVariableName $ Kore.variableName existsVariable
            , varSort = fromSort $ Kore.variableSort existsVariable
            , arg = fromPattern existsChild
            }
    FloorF Kore.Floor{floorOperandSort, floorResultSort, floorChild} ->
        KJFloor
            { argSort = fromSort floorOperandSort
            , resultSort = fromSort floorResultSort
            , arg = fromPattern floorChild
            }
    ForallF Kore.Forall{forallSort, forallVariable, forallChild} ->
        KJForall
            { sort = fromSort forallSort
            , var = fromKoreVariableName $ unElementVariableName $ variableName forallVariable
            , varSort = fromSort $ variableSort forallVariable
            , arg = fromPattern forallChild
            }
    IffF Kore.Iff{iffSort, iffFirst, iffSecond} ->
        KJIff
            { sort = fromSort iffSort
            , first =  fromPattern iffFirst
            , second = fromPattern iffSecond
            }
    ImpliesF Kore.Implies{impliesSort, impliesFirst, impliesSecond} ->
        KJImplies
            { sort = fromSort impliesSort
            , first = fromPattern impliesFirst
            , second = fromPattern impliesSecond
            }
    InF Kore.In{inOperandSort, inResultSort, inContainedChild, inContainingChild} ->
        KJIn
            { argSort = fromSort inOperandSort
            , resultSort = fromSort inResultSort
            , first = fromPattern inContainedChild
            , second = fromPattern inContainingChild
            }
    MuF Kore.Mu{muVariable, muChild} ->
        KJMu
            { var = fromKoreVariableName $ unSetVariableName $ variableName muVariable
            , varSort = fromSort $ variableSort muVariable
            , arg = fromPattern muChild
            }
    NextF Kore.Next{nextSort, nextChild} ->
        KJNext
            { sort = fromSort nextSort
            , dest = fromPattern nextChild
            }
    NotF Kore.Not{notSort, notChild} ->
        KJNot
            { sort = fromSort notSort
            , arg = fromPattern notChild
            }
    NuF Kore.Nu{nuVariable, nuChild} ->
        KJNu
            { var = fromKoreVariableName $ unSetVariableName $ variableName nuVariable
            , varSort = fromSort $ variableSort nuVariable
            , arg = fromPattern nuChild
            }
    OrF Kore.Or{orSort, orFirst, orSecond} ->
        KJOr
            { sort = fromSort orSort
            , first = fromPattern orFirst
            , second = fromPattern orSecond
            }
    RewritesF Kore.Rewrites{rewritesSort, rewritesFirst, rewritesSecond} ->
        KJRewrites
            { sort = fromSort rewritesSort
            , source = fromPattern rewritesFirst
            , dest = fromPattern rewritesSecond
            }
    TopF Kore.Top{topSort} ->
        KJTop { sort = fromSort topSort }
    InhabitantF Kore.Inhabitant{} ->
        error "Found inhabitant, not representable in json"
    StringLiteralF (Const Kore.StringLiteral{getStringLiteral}) ->
        KJString getStringLiteral
    VariableF (Const Kore.Variable{variableName, variableSort})
        | Kore.SomeVariableNameElement (ElementVariableName evar) <- variableName ->
            KJEVar{name = fromKoreVariableName evar, sort}
        | Kore.SomeVariableNameSet (SetVariableName svar) <- variableName ->
            KJSVar{name = fromKoreVariableName svar, sort}
      where
        sort = fromSort variableSort
  where
    fromSort :: Kore.Sort -> Sort
    fromSort = \case
        Kore.SortActualSort Kore.SortActual{sortActualName, sortActualSorts} ->
            Sort
                { name = fromKoreId sortActualName
                , args = map fromSort sortActualSorts
                }
        Kore.SortVariableSort Kore.SortVariable{getSortVariable} ->
            SortVariable $ Kore.getId getSortVariable

    fromKoreId :: Kore.Id -> Id
    fromKoreId =
        Id . Kore.getId -- forgetting the location
    fromKoreVariableName :: Kore.VariableName -> Id
    fromKoreVariableName VariableName{base, counter} =
        Id $
            Kore.getId base
                <> case counter of
                    Nothing -> ""
                    Just (Element n) -> T.pack $ show n
                    Just Sup -> error "Found Sup while converting variable name"
