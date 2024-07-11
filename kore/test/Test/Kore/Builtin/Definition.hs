{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Kore.Builtin.Definition where

import Data.Bifunctor qualified as Bifunctor
import Data.ByteString (
    ByteString,
 )
import Data.ByteString qualified as ByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.Default qualified as Default
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (
    fromJust,
 )
import Data.Sequence qualified as Seq
import Data.Text (
    Text,
 )
import Data.Word (
    Word8,
 )
import Kore.Attribute.Constructor
import Kore.Attribute.Hook
import Kore.Attribute.Injective
import Kore.Attribute.Parser
import Kore.Attribute.Sort.Concat qualified as Sort
import Kore.Attribute.Sort.Element qualified as Sort
import Kore.Attribute.Sort.HasDomainValues (
    hasDomainValuesAttribute,
 )
import Kore.Attribute.Sort.Unit qualified as Sort
import Kore.Attribute.SortInjection
import Kore.Attribute.Subsort (
    subsortAttribute,
 )
import Kore.Attribute.Synthetic (
    synthesize,
 )
import Kore.Attribute.Total
import Kore.Builtin.Endianness qualified as Endianness
import Kore.Builtin.Signedness qualified as Signedness
import Kore.Internal.ApplicationSorts
import Kore.Internal.InternalBool
import Kore.Internal.InternalBytes
import Kore.Internal.InternalInt
import Kore.Internal.InternalList
import Kore.Internal.InternalMap
import Kore.Internal.InternalSet
import Kore.Internal.InternalString
import Kore.Internal.Symbol (
    constructor,
    function,
    hook,
    injective,
    smthook,
    sortInjection,
    symbolKywd,
    total,
 )
import Kore.Internal.Symbol qualified as Internal
import Kore.Internal.TermLike hiding (
    Symbol,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Syntax (
    Const (..),
 )
import Kore.Syntax.Definition as Syntax
import Kore.Syntax.PatternF qualified as PatternF
import Prelude.Kore
import Test.Kore
import Test.Kore.Builtin.External
import Test.Kore.Rewrite.MockSymbols qualified as Mock

-- -------------------------------------------------------------

-- * Builtin symbols

-- | Make an unparameterized builtin symbol with the given name.
builtinSymbol :: Text -> Sort -> [Sort] -> Internal.Symbol
builtinSymbol name resultSort operandSorts =
    Internal.Symbol
        { symbolConstructor = testId name
        , symbolParams = []
        , symbolAttributes = Default.def
        , symbolSorts = applicationSorts operandSorts resultSort
        }
        & function

unarySymbol :: Text -> Sort -> Internal.Symbol
unarySymbol name sort = builtinSymbol name sort [sort]
binarySymbol :: Text -> Sort -> Internal.Symbol
binarySymbol name sort = builtinSymbol name sort [sort, sort]
-- ** Bool
binaryBoolSymbol :: Text -> Internal.Symbol
binaryBoolSymbol name = binarySymbol name boolSort
orBoolSymbol :: Internal.Symbol
orBoolSymbol = binaryBoolSymbol "orBool" & hook "BOOL.or" & smthook "or"
orElseBoolSymbol :: Internal.Symbol
orElseBoolSymbol = binaryBoolSymbol "orElseBool" & hook "BOOL.orElse"
andBoolSymbol :: Internal.Symbol
andBoolSymbol = binaryBoolSymbol "andBool" & hook "BOOL.and" & smthook "and"
andThenBoolSymbol :: Internal.Symbol
andThenBoolSymbol = binaryBoolSymbol "andThenBool" & hook "BOOL.andThen"
xorBoolSymbol :: Internal.Symbol
xorBoolSymbol = binaryBoolSymbol "xorBool" & hook "BOOL.xor" & smthook "xor"
neBoolSymbol :: Internal.Symbol
neBoolSymbol = binaryBoolSymbol "neBool" & hook "BOOL.ne" & smthook "distinct"
eqBoolSymbol :: Internal.Symbol
eqBoolSymbol = binaryBoolSymbol "eqBool" & hook "BOOL.eq" & smthook "="
notBoolSymbol :: Internal.Symbol
notBoolSymbol =
    unarySymbol "notBool" boolSort
        & hook "BOOL.not"
        & smthook "not"
impliesBoolSymbol :: Internal.Symbol
impliesBoolSymbol =
    binaryBoolSymbol "impliesBool"
        & hook "BOOL.implies"
        & smthook "=>"
notBool :: TermLike RewritingVariableName -> TermLike RewritingVariableName
notBool x = mkApplySymbol notBoolSymbol [x]
andBool
    , impliesBool
    , eqBool
    , orBool
    , andThenBool ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName
andBool x y = mkApplySymbol andBoolSymbol [x, y]
impliesBool x y = mkApplySymbol impliesBoolSymbol [x, y]
eqBool x y = mkApplySymbol eqBoolSymbol [x, y]
orBool x y = mkApplySymbol orBoolSymbol [x, y]
andThenBool x y = mkApplySymbol andThenBoolSymbol [x, y]
-- ** Int
comparisonSymbol :: Text -> Sort -> Internal.Symbol
comparisonSymbol name sort = builtinSymbol name boolSort [sort, sort]
comparisonIntSymbol :: Text -> Internal.Symbol
comparisonIntSymbol name = comparisonSymbol name intSort
unaryIntSymbol :: Text -> Internal.Symbol
unaryIntSymbol name = unarySymbol name intSort
binaryIntSymbol :: Text -> Internal.Symbol
binaryIntSymbol name = binarySymbol name intSort
gtIntSymbol :: Internal.Symbol
gtIntSymbol =
    comparisonIntSymbol "gtInt"
        & hook "INT.gt"
        & smthook ">"
        & function
        & total
geIntSymbol :: Internal.Symbol
geIntSymbol =
    comparisonIntSymbol "geInt"
        & hook "INT.ge"
        & smthook ">="
        & function
        & total
eqIntSymbol :: Internal.Symbol
eqIntSymbol =
    comparisonIntSymbol "eqInt"
        & hook "INT.eq"
        & smthook "="
        & function
        & total
leIntSymbol :: Internal.Symbol
leIntSymbol =
    comparisonIntSymbol "leInt"
        & hook "INT.le"
        & smthook "<="
        & function
        & total
ltIntSymbol :: Internal.Symbol
ltIntSymbol =
    comparisonIntSymbol "ltInt"
        & hook "INT.lt"
        & smthook "<"
        & function
        & total
neIntSymbol :: Internal.Symbol
neIntSymbol =
    comparisonIntSymbol "neInt"
        & hook "INT.ne"
        & smthook "distinct"
        & function
        & total
minIntSymbol :: Internal.Symbol
minIntSymbol =
    binaryIntSymbol "minInt"
        & hook "INT.min"
        & smthook "int_min"
        & function
        & total
maxIntSymbol :: Internal.Symbol
maxIntSymbol =
    binaryIntSymbol "maxInt"
        & hook "INT.max"
        & smthook "int_max"
        & function
        & total
addIntSymbol :: Internal.Symbol
addIntSymbol =
    binaryIntSymbol "addInt"
        & hook "INT.add"
        & smthook "+"
        & function
        & total
subIntSymbol :: Internal.Symbol
subIntSymbol =
    binaryIntSymbol "subInt"
        & hook "INT.sub"
        & smthook "-"
        & function
        & total
mulIntSymbol :: Internal.Symbol
mulIntSymbol =
    binaryIntSymbol "mulInt"
        & hook "INT.mul"
        & smthook "*"
        & function
        & total
absIntSymbol :: Internal.Symbol
absIntSymbol =
    unaryIntSymbol "absInt"
        & hook "INT.abs"
        & smthook "int_abs"
        & function
        & total
tdivIntSymbol :: Internal.Symbol
tdivIntSymbol =
    binaryIntSymbol "tdivInt"
        & hook "INT.tdiv"
        & smthook "div"
        & function
tmodIntSymbol :: Internal.Symbol
tmodIntSymbol =
    binaryIntSymbol "tmodInt"
        & hook "INT.tmod"
        & smthook "mod"
        & function
andIntSymbol :: Internal.Symbol
andIntSymbol = binaryIntSymbol "andInt" & hook "INT.and" & function & total
orIntSymbol :: Internal.Symbol
orIntSymbol = binaryIntSymbol "orInt" & hook "INT.or" & function & total
xorIntSymbol :: Internal.Symbol
xorIntSymbol = binaryIntSymbol "xorInt" & hook "INT.xor" & function & total
notIntSymbol :: Internal.Symbol
notIntSymbol = unaryIntSymbol "notInt" & hook "INT.not" & function & total
shlIntSymbol :: Internal.Symbol
shlIntSymbol = binaryIntSymbol "shlInt" & hook "INT.shl" & function & total
shrIntSymbol :: Internal.Symbol
shrIntSymbol = binaryIntSymbol "shrInt" & hook "INT.shr" & function & total
powIntSymbol :: Internal.Symbol
powIntSymbol = binaryIntSymbol "powInt" & hook "INT.pow" & function
powmodIntSymbol :: Internal.Symbol
powmodIntSymbol =
    builtinSymbol "powmodInt" intSort [intSort, intSort, intSort]
        & hook "INT.powmod"
        & function
log2IntSymbol :: Internal.Symbol
log2IntSymbol = unaryIntSymbol "log2Int" & hook "INT.log2" & function
edivIntSymbol :: Internal.Symbol
edivIntSymbol =
    binaryIntSymbol "edivInt"
        & hook "INT.ediv"
        & smthook "div"
        & function
emodIntSymbol :: Internal.Symbol
emodIntSymbol =
    binaryIntSymbol "emodInt"
        & hook "INT.emod"
        & smthook "mod"
        & function

-- an unhooked, uninterpreted function f : Int -> Int
dummyIntSymbol :: Internal.Symbol
dummyIntSymbol = unaryIntSymbol "f" & function
dummyInt ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
dummyInt x = mkApplySymbol dummyIntSymbol [x]
dummyFunctionalIntSymbol :: Internal.Symbol
dummyFunctionalIntSymbol = unaryIntSymbol "ff" & function & total
dummyFunctionalInt ::
    TermLike RewritingVariableName -> TermLike RewritingVariableName
dummyFunctionalInt x = mkApplySymbol dummyFunctionalIntSymbol [x]
addInt
    , subInt
    , mulInt
    , divInt
    , tdivInt
    , tmodInt ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName
addInt i j = mkApplySymbol addIntSymbol [i, j]
subInt i j = mkApplySymbol subIntSymbol [i, j]
mulInt i j = mkApplySymbol mulIntSymbol [i, j]
divInt i j = mkApplySymbol tdivIntSymbol [i, j]
tdivInt i j = mkApplySymbol tdivIntSymbol [i, j]
tmodInt i j = mkApplySymbol tmodIntSymbol [i, j]
eqInt
    , ltInt ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName
eqInt i j = mkApplySymbol eqIntSymbol [i, j]
ltInt i j = mkApplySymbol ltIntSymbol [i, j]
-- ** KEQUAL
comparisonKSymbol :: Text -> Internal.Symbol
comparisonKSymbol name = comparisonSymbol name kSort
keqBoolSymbol :: Internal.Symbol
keqBoolSymbol = comparisonKSymbol "keqBool" & hook "KEQUAL.eq"
kneqBoolSymbol :: Internal.Symbol
kneqBoolSymbol = comparisonKSymbol "kneqBool" & hook "KEQUAL.ne"
kiteKSymbol :: Internal.Symbol
kiteKSymbol =
    builtinSymbol "kiteK" kSort [boolSort, kSort, kSort]
        & hook "KEQUAL.ite"
kseqSymbol :: Internal.Symbol
kseqSymbol =
    builtinSymbol "kseq" kSort [kItemSort, kSort]
        & constructor
dotkSymbol :: Internal.Symbol
dotkSymbol = builtinSymbol "dotk" kSort [] & constructor
injSymbol :: Sort -> Sort -> Internal.Symbol
injSymbol lSort rSort =
    Internal.Symbol
        { symbolConstructor = testId "inj"
        , symbolParams = [lSort, rSort]
        , symbolAttributes = Default.def
        , symbolSorts = applicationSorts [lSort] rSort
        }
        & sortInjection
        & injective
inj ::
    InternalVariable variable =>
    Sort ->
    TermLike variable ->
    TermLike variable
inj injTo injChild =
    (synthesize . InjF)
        Inj{injConstructor, injFrom, injTo, injAttributes, injChild}
  where
    injFrom = termLikeSort injChild
    symbol = injSymbol injFrom injTo
    Internal.Symbol{symbolConstructor = injConstructor} = symbol
    Internal.Symbol{symbolAttributes = injAttributes} = symbol
keqBool
    , kneqBool ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName
keqBool x y = mkApplySymbol keqBoolSymbol [x, y]
kneqBool x y = mkApplySymbol kneqBoolSymbol [x, y]
kseq ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
kseq x y = mkApplySymbol kseqSymbol [x, y]
dotk :: TermLike RewritingVariableName
dotk = mkApplySymbol dotkSymbol []
kiteK ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
kiteK i t e = mkApplySymbol kiteKSymbol [i, t, e]
-- ** List
unitListSymbol :: Internal.Symbol
unitListSymbol = builtinSymbol "unitList" listSort [] & hook "LIST.unit"
unitList2Symbol :: Internal.Symbol
unitList2Symbol = builtinSymbol "unitList2" listSort2 [] & hook "LIST.unit"
elementListSymbol :: Internal.Symbol
elementListSymbol =
    builtinSymbol "elementList" listSort [intSort]
        & hook "LIST.element"
        & total
elementList2Symbol :: Internal.Symbol
elementList2Symbol =
    builtinSymbol "elementList2" listSort2 [intSort]
        & hook "LIST.element"
        & total
concatListSymbol :: Internal.Symbol
concatListSymbol =
    binarySymbol "concatList" listSort & hook "LIST.concat" & total
concatList2Symbol :: Internal.Symbol
concatList2Symbol =
    binarySymbol "concatList2" listSort2 & hook "LIST.concat" & total
getListSymbol :: Internal.Symbol
getListSymbol =
    builtinSymbol "getList" intSort [listSort, intSort] & hook "LIST.get"
sizeListSymbol :: Internal.Symbol
sizeListSymbol = builtinSymbol "sizeList" intSort [listSort] & hook "LIST.size"
makeListSymbol :: Internal.Symbol
makeListSymbol =
    builtinSymbol "makeList" listSort [intSort, intSort] & hook "LIST.make"
updateListSymbol :: Internal.Symbol
updateListSymbol =
    builtinSymbol
        "updateList"
        listSort
        [listSort, intSort, intSort]
        & hook "LIST.update"
inListSymbol :: Internal.Symbol
inListSymbol = builtinSymbol "inList" boolSort [intSort, listSort] & hook "LIST.in"
updateAllListSymbol :: Internal.Symbol
updateAllListSymbol =
    builtinSymbol "updateAllList" listSort [listSort, intSort, listSort]
        & hook "LIST.updateAll"
unitList :: TermLike RewritingVariableName
unitList = mkApplySymbol unitListSymbol []
elementList ::
    TermLike RewritingVariableName -> TermLike RewritingVariableName
elementList x = mkApplySymbol elementListSymbol [x]
concatList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
concatList x y = mkApplySymbol concatListSymbol [x, y]
getList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
getList list poz = mkApplySymbol getListSymbol [list, poz]
sizeList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
sizeList l = mkApplySymbol sizeListSymbol [l]
makeList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
makeList len x = mkApplySymbol makeListSymbol [len, x]
updateList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
updateList list poz value = mkApplySymbol updateListSymbol [list, poz, value]
inList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
inList x list = mkApplySymbol inListSymbol [x, list]
updateAllList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
updateAllList l1 ix l2 = mkApplySymbol updateAllListSymbol [l1, ix, l2]
-- ** Map
unitMapSymbol :: Internal.Symbol
unitMapSymbol = builtinSymbol "unitMap" mapSort [] & hook "MAP.unit"
updateMapSymbol :: Internal.Symbol
updateMapSymbol =
    builtinSymbol "updateMap" mapSort [mapSort, intSort, intSort]
        & hook "MAP.update"
updateAllMapSymbol :: Internal.Symbol
updateAllMapSymbol =
    builtinSymbol "updateAllMap" mapSort [mapSort, mapSort]
        & hook "MAP.updateAll"
lookupMapSymbol :: Internal.Symbol
lookupMapSymbol =
    builtinSymbol "lookupMap" intSort [mapSort, intSort]
        & hook "MAP.lookup"
lookupOrDefaultMapSymbol :: Internal.Symbol
lookupOrDefaultMapSymbol =
    builtinSymbol "lookupOrDefaultMap" intSort [mapSort, intSort, intSort]
        & hook "MAP.lookupOrDefault"
elementMapSymbol :: Internal.Symbol
elementMapSymbol =
    builtinSymbol "elementMap" mapSort [intSort, intSort]
        & hook "MAP.element"
        & total
concatMapSymbol :: Internal.Symbol
concatMapSymbol =
    binarySymbol "concatMap" mapSort & hook "MAP.concat" & function
inKeysMapSymbol :: Internal.Symbol
inKeysMapSymbol =
    builtinSymbol "inKeysMap" boolSort [intSort, mapSort]
        & hook "MAP.in_keys"
keysMapSymbol :: Internal.Symbol
keysMapSymbol =
    builtinSymbol "keysMap" setSort [mapSort] & hook "MAP.keys"
keysListMapSymbol :: Internal.Symbol
keysListMapSymbol =
    builtinSymbol "keysListMap" listSort [mapSort] & hook "MAP.keys_list"
removeMapSymbol :: Internal.Symbol
removeMapSymbol =
    builtinSymbol "removeMap" mapSort [mapSort, intSort] & hook "MAP.remove"
removeAllMapSymbol :: Internal.Symbol
removeAllMapSymbol =
    builtinSymbol "removeAllMap" mapSort [mapSort, setSort]
        & hook "MAP.removeAll"
sizeMapSymbol :: Internal.Symbol
sizeMapSymbol =
    builtinSymbol "sizeMap" intSort [mapSort] & hook "MAP.size"
valuesMapSymbol :: Internal.Symbol
valuesMapSymbol =
    builtinSymbol "valuesMap" listSort [mapSort] & hook "MAP.values"
inclusionMapSymbol :: Internal.Symbol
inclusionMapSymbol =
    builtinSymbol "inclusionMap" boolSort [mapSort, mapSort] & hook "MAP.inclusion"
unitMap :: TermLike RewritingVariableName
unitMap = mkApplySymbol unitMapSymbol []
updateMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
updateMap map' key value = mkApplySymbol updateMapSymbol [map', key, value]
updateAllMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
updateAllMap map' updates = mkApplySymbol updateAllMapSymbol [map', updates]
lookupMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
lookupMap map' key = mkApplySymbol lookupMapSymbol [map', key]
lookupOrDefaultMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
lookupOrDefaultMap map' key def' =
    mkApplySymbol lookupOrDefaultMapSymbol [map', key, def']
elementMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
elementMap key value = mkApplySymbol elementMapSymbol [key, value]
concatMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
concatMap map1 map2 = mkApplySymbol concatMapSymbol [map1, map2]
inKeysMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
inKeysMap key map' = mkApplySymbol inKeysMapSymbol [key, map']
keysMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
keysMap map' = mkApplySymbol keysMapSymbol [map']
keysListMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
keysListMap map' = mkApplySymbol keysListMapSymbol [map']
removeMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
removeMap map' key = mkApplySymbol removeMapSymbol [map', key]
removeAllMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
removeAllMap map' set = mkApplySymbol removeAllMapSymbol [map', set]
sizeMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
sizeMap map' = mkApplySymbol sizeMapSymbol [map']
valuesMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
valuesMap map' = mkApplySymbol valuesMapSymbol [map']
inclusionMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
inclusionMap mapLeft mapRight = mkApplySymbol inclusionMapSymbol [mapLeft, mapRight]
-- ** Pair
pairSymbol :: Sort -> Sort -> Internal.Symbol
pairSymbol lSort rSort =
    Internal.Symbol
        { symbolConstructor = testId "pair"
        , symbolParams = [lSort, rSort]
        , symbolAttributes = Default.def
        , symbolSorts = applicationSorts [lSort, rSort] (pairSort lSort rSort)
        }
        & constructor
        & total
pair ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
pair l r =
    mkApplySymbol (pairSymbol lSort rSort) [l, r]
  where
    lSort = termLikeSort l
    rSort = termLikeSort r
-- ** Set
unitSetSymbol :: Internal.Symbol
unitSetSymbol = builtinSymbol "unitSet" setSort [] & hook "SET.unit"
unitSet :: TermLike RewritingVariableName
unitSet = mkApplySymbol unitSetSymbol []
elementSetSymbol :: Internal.Symbol
elementSetSymbol =
    builtinSymbol "elementSet" setSort [intSort]
        & hook "SET.element"
        & total
elementSetSymbolTestSort :: Internal.Symbol
elementSetSymbolTestSort =
    builtinSymbol "elementSet" setSort [Mock.testSort]
        & hook "SET.element"
        & total
elementSet ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
elementSet x = mkApplySymbol elementSetSymbol [x]
concatSetSymbol :: Internal.Symbol
concatSetSymbol =
    binarySymbol "concatSet" setSort & hook "SET.concat" & function
concatSet ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
concatSet s1 s2 = mkApplySymbol concatSetSymbol [s1, s2]
inSetSymbol :: Internal.Symbol
inSetSymbol =
    builtinSymbol "inSet" boolSort [intSort, setSort] & hook "SET.in"
inSetSymbolTestSort :: Internal.Symbol
inSetSymbolTestSort =
    builtinSymbol "inSet" boolSort [Mock.testSort, setSort] & hook "SET.in"
differenceSetSymbol :: Internal.Symbol
differenceSetSymbol =
    binarySymbol "differenceSet" setSort & hook "SET.difference"
differenceSet ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
differenceSet set1 set2 = mkApplySymbol differenceSetSymbol [set1, set2]
toListSetSymbol :: Internal.Symbol
toListSetSymbol =
    builtinSymbol "toListSet" listSort [setSort] & hook "SET.set2list"
sizeSetSymbol :: Internal.Symbol
sizeSetSymbol = builtinSymbol "sizeSet" intSort [setSort] & hook "SET.size"
intersectionSetSymbol :: Internal.Symbol
intersectionSetSymbol =
    binarySymbol "intersectionSet" setSort & hook "SET.intersection"
list2setSetSymbol :: Internal.Symbol
list2setSetSymbol =
    builtinSymbol "list2setSet" setSort [listSort] & hook "SET.list2set"
inclusionSetSymbol :: Internal.Symbol
inclusionSetSymbol =
    builtinSymbol "inclusionSet" boolSort [setSort, setSort]
        & hook "SET.inclusion"
intersectionSet ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
intersectionSet set1 set2 =
    mkApplySymbol intersectionSetSymbol [set1, set2]
list2setSet ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
list2setSet list =
    mkApplySymbol list2setSetSymbol [list]
inclusionSet ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
inclusionSet setLeft setRight =
    mkApplySymbol inclusionSetSymbol [setLeft, setRight]
-- ** IO
logStringSymbol :: Internal.Symbol
logStringSymbol =
    builtinSymbol "logString" kSort [stringSort]
        & hook "IO.logString"
        & hook "IO.logString"
        & hook "IO.logString"
        & hook "IO.logString"
-- ** String
eqStringSymbol :: Internal.Symbol
eqStringSymbol =
    comparisonSymbol "eqString" stringSort & hook "STRING.eq"
ltStringSymbol :: Internal.Symbol
ltStringSymbol =
    comparisonSymbol "ltString" stringSort & hook "STRING.lt"
concatStringSymbol :: Internal.Symbol
concatStringSymbol =
    binarySymbol "concatString" stringSort & hook "STRING.concat"
substrStringSymbol :: Internal.Symbol
substrStringSymbol =
    builtinSymbol "substrString" stringSort [stringSort, intSort, intSort]
        & hook "STRING.substr"
lengthStringSymbol :: Internal.Symbol
lengthStringSymbol =
    builtinSymbol "lengthString" intSort [stringSort] & hook "STRING.length"
chrStringSymbol :: Internal.Symbol
chrStringSymbol =
    builtinSymbol "chrString" stringSort [intSort] & hook "STRING.chr"
ordStringSymbol :: Internal.Symbol
ordStringSymbol =
    builtinSymbol "ordString" intSort [stringSort] & hook "STRING.ord"
findStringSymbol :: Internal.Symbol
findStringSymbol =
    builtinSymbol "findString" intSort [stringSort, stringSort, intSort]
        & hook "STRING.find"
string2BaseStringSymbol :: Internal.Symbol
string2BaseStringSymbol =
    builtinSymbol "string2baseString" intSort [stringSort, intSort]
        & hook "STRING.string2base"
base2StringStringSymbol :: Internal.Symbol
base2StringStringSymbol =
    builtinSymbol "base2stringString" stringSort [intSort, intSort]
        & hook "STRING.base2string"
string2IntStringSymbol :: Internal.Symbol
string2IntStringSymbol =
    builtinSymbol "string2intString" intSort [stringSort]
        & hook "STRING.string2int"
int2StringStringSymbol :: Internal.Symbol
int2StringStringSymbol =
    builtinSymbol "int2stringString" stringSort [intSort]
        & hook "STRING.int2string"
token2StringStringSymbol :: Internal.Symbol
token2StringStringSymbol =
    builtinSymbol "token2stringString" stringSort [userTokenSort]
        & hook "STRING.token2string"
string2TokenStringSymbol :: Internal.Symbol
string2TokenStringSymbol =
    builtinSymbol "string2tokenString" userTokenSort [stringSort]
        & hook "STRING.string2token"
eqString
    , concatString ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName
eqString i j = mkApplySymbol eqStringSymbol [i, j]
concatString x y = mkApplySymbol concatStringSymbol [x, y]
-- * Bytes
littleEndianBytesSymbol :: Internal.Symbol
littleEndianBytesSymbol =
    builtinSymbol "littleEndianBytes" endiannessSort []
        & symbolKywd "littleEndianBytes"
littleEndianBytes :: InternalVariable variable => TermLike variable
littleEndianBytes =
    mkEndianness (Endianness.LittleEndian littleEndianBytesSymbol)
bigEndianBytesSymbol :: Internal.Symbol
bigEndianBytesSymbol =
    builtinSymbol "bigEndianBytes" endiannessSort []
        & symbolKywd "bigEndianBytes"
bigEndianBytes :: InternalVariable variable => TermLike variable
bigEndianBytes =
    mkEndianness (Endianness.BigEndian bigEndianBytesSymbol)
signedBytesSymbol :: Internal.Symbol
signedBytesSymbol =
    builtinSymbol "signedBytes" signednessSort []
        & symbolKywd "signedBytes"
signedBytes :: InternalVariable variable => TermLike variable
signedBytes =
    mkSignedness (Signedness.Signed signedBytesSymbol)
unsignedBytesSymbol :: Internal.Symbol
unsignedBytesSymbol =
    builtinSymbol "unsignedBytes" signednessSort []
        & symbolKywd "unsignedBytes"
unsignedBytes :: InternalVariable variable => TermLike variable
unsignedBytes =
    mkSignedness (Signedness.Unsigned unsignedBytesSymbol)
bytes2stringBytesSymbol :: Internal.Symbol
bytes2stringBytesSymbol =
    builtinSymbol "bytes2stringBytes" stringSort [bytesSort]
        & hook "BYTES.bytes2string"
string2bytesBytesSymbol :: Internal.Symbol
string2bytesBytesSymbol =
    builtinSymbol "string2bytesBytes" bytesSort [stringSort]
        & hook "BYTES.string2bytes"
decodeBytesBytesSymbol :: Internal.Symbol
decodeBytesBytesSymbol =
    builtinSymbol "decodeBytesBytes" stringSort [stringSort, bytesSort]
        & hook "BYTES.decodeBytes"
encodeBytesBytesSymbol :: Internal.Symbol
encodeBytesBytesSymbol =
    builtinSymbol "encodeBytesBytes" bytesSort [stringSort, stringSort]
        & hook "BYTES.encodeBytes"
updateBytesSymbol :: Internal.Symbol
updateBytesSymbol =
    builtinSymbol "updateBytes" bytesSort [bytesSort, intSort, intSort]
        & hook "BYTES.update"
getBytesSymbol :: Internal.Symbol
getBytesSymbol =
    builtinSymbol "getBytes" intSort [bytesSort, intSort]
        & hook "BYTES.get"
substrBytesSymbol :: Internal.Symbol
substrBytesSymbol =
    builtinSymbol "substrBytes" bytesSort [bytesSort, intSort, intSort]
        & hook "BYTES.substr"
replaceAtBytesSymbol :: Internal.Symbol
replaceAtBytesSymbol =
    builtinSymbol "replaceAtBytes" bytesSort [bytesSort, intSort, bytesSort]
        & hook "BYTES.replaceAt"
padRightBytesSymbol :: Internal.Symbol
padRightBytesSymbol =
    builtinSymbol "padRightBytes" bytesSort [bytesSort, intSort, intSort]
        & hook "BYTES.padRight"
padLeftBytesSymbol :: Internal.Symbol
padLeftBytesSymbol =
    builtinSymbol "padLeftBytes" bytesSort [bytesSort, intSort, intSort]
        & hook "BYTES.padLeft"
reverseBytesSymbol :: Internal.Symbol
reverseBytesSymbol =
    builtinSymbol "reverseBytes" bytesSort [bytesSort]
        & hook "BYTES.reverse"
lengthBytesSymbol :: Internal.Symbol
lengthBytesSymbol =
    builtinSymbol "lengthBytes" intSort [bytesSort]
        & hook "BYTES.length"
concatBytesSymbol :: Internal.Symbol
concatBytesSymbol =
    builtinSymbol "concatBytes" bytesSort [bytesSort, bytesSort]
        & hook "BYTES.concat"
int2bytesSymbol :: Internal.Symbol
int2bytesSymbol =
    builtinSymbol "int2bytes" bytesSort [intSort, intSort, endiannessSort]
        & hook "BYTES.int2bytes"
int2bytes ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
int2bytes len i end = mkApplySymbol int2bytesSymbol [len, i, end]
bytes2intSymbol :: Internal.Symbol
bytes2intSymbol =
    builtinSymbol
        "bytes1int"
        intSort
        [bytesSort, endiannessSort, signednessSort]
        & hook "BYTES.bytes2int"
bytes2int ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
bytes2int bytes end sign = mkApplySymbol bytes2intSymbol [bytes, end, sign]
-- * Krypto
ecdsaRecoverSymbol :: Internal.Symbol
ecdsaRecoverSymbol =
    builtinSymbol
        "ecdsaRecoverKrypto"
        bytesSort
        [bytesSort, intSort, bytesSort, bytesSort]
        & hook "KRYPTO.ecdsaRecover"
keccak256Symbol :: Internal.Symbol
keccak256Symbol =
    builtinSymbol "keccak256Krypto" stringSort [bytesSort]
        & hook "KRYPTO.keccak256"
keccak256RawSymbol :: Internal.Symbol
keccak256RawSymbol =
    builtinSymbol "keccak256RawKrypto" bytesSort [bytesSort]
        & hook "KRYPTO.keccak256raw"
sha256Symbol :: Internal.Symbol
sha256Symbol =
    builtinSymbol "sha256Krypto" stringSort [bytesSort]
        & hook "KRYPTO.sha256"
sha3256Symbol :: Internal.Symbol
sha3256Symbol =
    builtinSymbol "sha3256Krypto" stringSort [bytesSort]
        & hook "KRYPTO.sha3256"
ripemd160Symbol :: Internal.Symbol
ripemd160Symbol =
    builtinSymbol "ripemd160Krypto" stringSort [bytesSort]
        & hook "KRYPTO.ripemd160"
ecdsaRecoverKrypto ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
ecdsaRecoverKrypto m v r s = mkApplySymbol ecdsaRecoverSymbol [m, v, r, s]
keccak256Krypto ::
    TermLike RewritingVariableName -> TermLike RewritingVariableName
keccak256Krypto message = mkApplySymbol keccak256Symbol [message]
keccak256RawKrypto ::
    TermLike RewritingVariableName -> TermLike RewritingVariableName
keccak256RawKrypto message = mkApplySymbol keccak256RawSymbol [message]
sha256Krypto ::
    TermLike RewritingVariableName -> TermLike RewritingVariableName
sha256Krypto message = mkApplySymbol sha256Symbol [message]
sha3256Krypto ::
    TermLike RewritingVariableName -> TermLike RewritingVariableName
sha3256Krypto message = mkApplySymbol sha3256Symbol [message]
ripemd160Krypto ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
ripemd160Krypto message = mkApplySymbol ripemd160Symbol [message]

-- -------------------------------------------------------------

-- * Sorts

-- | Declare 'a sort' in a Kore module.
sortDecl :: Sort -> ParsedSentence
sortDecl sort =
    asSentence sentence
  where
    sentence :: ParsedSentenceSort
    sentence =
        SentenceSort
            { sentenceSortName =
                let SortActualSort SortActual{sortActualName} = sort
                 in sortActualName
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes []
            }

sortDeclWithAttributes ::
    -- | declared sort
    Sort ->
    -- | declaration attributes
    [ParsedPattern] ->
    ParsedSentence
sortDeclWithAttributes sort attributes =
    asSentence sentence
  where
    sentence :: ParsedSentenceSort
    sentence =
        SentenceSort
            { sentenceSortName =
                let SortActualSort SortActual{sortActualName} = sort
                 in sortActualName
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes attributes
            }

-- | Declare a hooked sort.
hookedSortDecl ::
    -- | declared sort
    Sort ->
    -- | declaration attributes
    [ParsedPattern] ->
    ParsedSentence
hookedSortDecl sort attrs =
    (asSentence . SentenceHookedSort) sentence
  where
    sentence :: ParsedSentenceSort
    sentence =
        SentenceSort
            { sentenceSortName =
                let SortActualSort SortActual{sortActualName} = sort
                 in sortActualName
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes attrs
            }

-- ** Bool

-- | A sort to hook to the builtin @BOOL.Bool@.
boolSort :: Sort
boolSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Bool"
            , sortActualSorts = []
            }

-- | Declare 'boolSort' in a Kore module.
boolSortDecl :: ParsedSentence
boolSortDecl =
    hookedSortDecl
        boolSort
        [hasDomainValuesAttribute, hookAttribute "BOOL.Bool"]

builtinBool :: Bool -> InternalBool
builtinBool internalBoolValue =
    InternalBool
        { internalBoolSort = boolSort
        , internalBoolValue
        }
mkBool :: InternalVariable variable => Bool -> TermLike variable
mkBool = mkInternalBool . builtinBool
-- ** Int

-- | A sort to hook to the builtin @INT.Int@.
intSort :: Sort
intSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Int"
            , sortActualSorts = []
            }

-- | Declare 'intSort' in a Kore module.
intSortDecl :: ParsedSentence
intSortDecl =
    hookedSortDecl
        intSort
        [hasDomainValuesAttribute, hookAttribute "INT.Int"]

builtinInt :: Integer -> InternalInt
builtinInt internalIntValue =
    InternalInt
        { internalIntSort = intSort
        , internalIntValue
        }
mkInt :: InternalVariable variable => Integer -> TermLike variable
mkInt = mkInternalInt . builtinInt
-- ** KEQUAL
kSort :: Sort
kSort =
    SortActualSort
        SortActual
            { sortActualName = testId "SortK"
            , sortActualSorts = []
            }
kItemSort :: Sort
kItemSort =
    SortActualSort
        SortActual
            { sortActualName = testId "SortKItem"
            , sortActualSorts = []
            }
idSort :: Sort
idSort =
    SortActualSort
        SortActual
            { sortActualName = testId "SortId"
            , sortActualSorts = []
            }
-- ** List

-- | A sort to hook to the builtin @LIST.List@.
listSort :: Sort
listSort =
    SortActualSort
        SortActual
            { sortActualName = testId "List"
            , sortActualSorts = []
            }

-- | Declare 'listSort' in a Kore module.
listSortDecl :: ParsedSentence
listSortDecl =
    hookedSortDecl
        listSort
        [ hookAttribute "LIST.List"
        , unitAttribute unitListSymbol
        , elementAttribute elementListSymbol
        , concatAttribute concatListSymbol
        ]

builtinList :: [TermLike variable] -> InternalList (TermLike variable)
builtinList children =
    InternalList
        { internalListSort = listSort
        , internalListUnit = unitListSymbol
        , internalListElement = elementListSymbol
        , internalListConcat = concatListSymbol
        , internalListChild = Seq.fromList children
        }
mkList :: InternalVariable variable => [TermLike variable] -> TermLike variable
mkList = mkInternalList . builtinList

-- | Another sort with the same hook
listSort2 :: Sort
listSort2 =
    SortActualSort
        SortActual
            { sortActualName = testId "List2"
            , sortActualSorts = []
            }

-- | Declare 'listSort' in a Kore module.
listSortDecl2 :: ParsedSentence
listSortDecl2 =
    hookedSortDecl
        listSort2
        [ hookAttribute "LIST.List"
        , unitAttribute unitList2Symbol
        , elementAttribute elementList2Symbol
        , concatAttribute concatList2Symbol
        ]

-- ** Map

-- | A sort to hook to the builtin @MAP.Map@.
mapSort :: Sort
mapSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Map"
            , sortActualSorts = []
            }

-- | Declare 'mapSort' in a Kore module.
mapSortDecl :: ParsedSentence
mapSortDecl =
    hookedSortDecl
        mapSort
        [ hookAttribute "MAP.Map"
        , unitAttribute unitMapSymbol
        , elementAttribute elementMapSymbol
        , concatAttribute concatMapSymbol
        ]

builtinMap ::
    Ord key =>
    Hashable key =>
    [(key, TermLike variable)] ->
    InternalMap key (TermLike variable)
builtinMap children =
    InternalAc
        { builtinAcSort = mapSort
        , builtinAcUnit = unitMapSymbol
        , builtinAcElement = elementMapSymbol
        , builtinAcConcat = concatMapSymbol
        , builtinAcChild =
            NormalizedMap
                NormalizedAc
                    { elementsWithVariables = []
                    , concreteElements =
                        HashMap.fromList (Bifunctor.second MapValue <$> children)
                    , opaque = []
                    }
        }
mkMap ::
    InternalVariable variable =>
    [(TermLike Concrete, TermLike variable)] ->
    TermLike variable
mkMap =
    mkInternalMap
        . builtinMap
        . (map . Bifunctor.first) (retractKey >>> fromJust)
-- ** Pair
pairSort :: Sort -> Sort -> Sort
pairSort lSort rSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Pair"
            , sortActualSorts = [lSort, rSort]
            }

-- | Declare 'Pair' in a Kore module.
pairSortDecl :: ParsedSentence
pairSortDecl =
    asSentence decl
  where
    lSortVariable = SortVariable (testId "l")
    rSortVariable = SortVariable (testId "r")
    lSort = SortVariableSort lSortVariable
    rSort = SortVariableSort rSortVariable
    decl :: ParsedSentenceSort
    decl =
        SentenceSort
            { sentenceSortName =
                let SortActualSort SortActual{sortActualName} =
                        pairSort lSort rSort
                 in sortActualName
            , sentenceSortParameters = [lSortVariable, rSortVariable]
            , sentenceSortAttributes = Attributes []
            }

-- ** Set

-- | A sort to hook to the builtin @SET.Set@.
setSort :: Sort
setSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Set"
            , sortActualSorts = []
            }

-- | Declare 'setSort' in a Kore module.
setSortDecl :: ParsedSentence
setSortDecl =
    hookedSortDecl
        setSort
        [ hookAttribute "SET.Set"
        , unitAttribute unitSetSymbol
        , elementAttribute elementSetSymbol
        , concatAttribute concatSetSymbol
        ]

testSort :: Sort
testSort =
    SortActualSort
        SortActual
            { sortActualName = testId "testSort"
            , sortActualSorts = []
            }
testSortDecl :: ParsedSentence
testSortDecl = sortDecl testSort
mkSet ::
    InternalVariable variable =>
    Foldable f =>
    f (TermLike variable) ->
    [TermLike variable] ->
    TermLike variable
mkSet elements opaque =
    mkInternalSet
        InternalAc
            { builtinAcSort = setSort
            , builtinAcUnit = unitSetSymbol
            , builtinAcElement = elementSetSymbol
            , builtinAcConcat = concatSetSymbol
            , builtinAcChild =
                NormalizedSet
                    NormalizedAc
                        { elementsWithVariables = wrapElement <$> abstractElements
                        , concreteElements
                        , opaque
                        }
            }
  where
    asKey key =
        (,) <$> retractKey key <*> pure SetValue
            & maybe (Left (key, SetValue)) Right
    (abstractElements, HashMap.fromList -> concreteElements) =
        asKey <$> toList elements
            & partitionEithers
mkSet_ ::
    InternalVariable variable =>
    Foldable f =>
    f (TermLike variable) ->
    TermLike variable
mkSet_ items = mkSet items []
-- ** String

-- | A sort to hook to the builtin @STRING.String@.
stringSort :: Sort
stringSort =
    SortActualSort
        SortActual
            { sortActualName = testId "String"
            , sortActualSorts = []
            }

-- | A user defined token sort
userTokenSort :: Sort
userTokenSort =
    SortActualSort
        SortActual
            { sortActualName = testId "UserToken"
            , sortActualSorts = []
            }

-- | Declare 'stringSort' in a Kore module.
stringSortDecl :: ParsedSentence
stringSortDecl =
    hookedSortDecl
        stringSort
        [hasDomainValuesAttribute, hookAttribute "STRING.String"]

-- | Declare a user defined token sort in a Kore module
userTokenSortDecl :: ParsedSentence
userTokenSortDecl =
    sortDeclWithAttributes
        userTokenSort
        [hasDomainValuesAttribute]

mkString :: InternalVariable variable => Text -> TermLike variable
mkString = mkInternalString . internalString
internalString :: Text -> InternalString
internalString internalStringValue =
    InternalString
        { internalStringSort = stringSort
        , internalStringValue
        }
-- ** Bytes
bytesSort :: Sort
bytesSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Bytes"
            , sortActualSorts = []
            }
bytesSortDecl :: ParsedSentence
bytesSortDecl =
    hookedSortDecl
        bytesSort
        [ hookAttribute "BYTES.Bytes"
        , hasDomainValuesAttribute
        ]
endiannessSort :: Sort
endiannessSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Endianness"
            , sortActualSorts = []
            }
endiannessSortDecl :: ParsedSentence
endiannessSortDecl = sortDecl endiannessSort
signednessSort :: Sort
signednessSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Signedness"
            , sortActualSorts = []
            }
signednessSortDecl :: ParsedSentence
signednessSortDecl = sortDecl signednessSort
builtinBytes :: ByteString -> InternalBytes
builtinBytes byteString =
    InternalBytes
        { internalBytesSort = bytesSort
        , internalBytesValue = ShortByteString.toShort byteString
        }
mkBytes :: InternalVariable variable => [Word8] -> TermLike variable
mkBytes = mkInternalBytes' . builtinBytes . ByteString.pack

-- -------------------------------------------------------------

-- * Modules
unitAttribute
    , elementAttribute
    , concatAttribute ::
        Internal.Symbol -> AttributePattern
unitAttribute = Sort.unitAttribute . Internal.toSymbolOrAlias
elementAttribute = Sort.elementAttribute . Internal.toSymbolOrAlias
concatAttribute = Sort.concatAttribute . Internal.toSymbolOrAlias

-- | Declare a symbol hooked to the given builtin name.
hookedSymbolDecl :: Internal.Symbol -> ParsedSentence
hookedSymbolDecl symbol =
    (asSentence . SentenceHookedSymbol) sentence
  where
    Internal.Symbol{symbolConstructor, symbolAttributes, symbolSorts} =
        symbol
    sentence :: ParsedSentenceSymbol
    sentence =
        SentenceSymbol
            { sentenceSymbolSymbol
            , sentenceSymbolSorts
            , sentenceSymbolResultSort
            , sentenceSymbolAttributes = toAttributes symbolAttributes
            }
    sentenceSymbolSymbol =
        Symbol
            { symbolConstructor = symbolConstructor
            , symbolParams = []
            }
    sentenceSymbolSorts = applicationSortsOperands symbolSorts
    sentenceSymbolResultSort = applicationSortsResult symbolSorts

symbolDecl :: Internal.Symbol -> ParsedSentence
symbolDecl symbol =
    asSentence sentence
  where
    Internal.Symbol{symbolConstructor, symbolAttributes, symbolSorts} =
        symbol
    sentence :: ParsedSentenceSymbol
    sentence =
        SentenceSymbol
            { sentenceSymbolSymbol
            , sentenceSymbolSorts
            , sentenceSymbolResultSort
            , sentenceSymbolAttributes = toAttributes symbolAttributes
            }
    sentenceSymbolSymbol =
        Symbol
            { symbolConstructor = symbolConstructor
            , symbolParams = []
            }
    sentenceSymbolSorts = applicationSortsOperands symbolSorts
    sentenceSymbolResultSort = applicationSortsResult symbolSorts
importParsedModule :: ModuleName -> ParsedSentence
importParsedModule moduleName =
    asSentence sentence
  where
    sentence :: ParsedSentenceImport
    sentence =
        SentenceImport
            { sentenceImportModuleName = moduleName
            , sentenceImportAttributes = Attributes []
            }
-- ** BOOL

boolModuleName :: ModuleName
boolModuleName = ModuleName "BOOL"

-- | Declare the @BOOL@ builtins.
boolModule :: ParsedModule
boolModule =
    Module
        { moduleName = boolModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ boolSortDecl
            , hookedSymbolDecl orBoolSymbol
            , hookedSymbolDecl orElseBoolSymbol
            , hookedSymbolDecl andBoolSymbol
            , hookedSymbolDecl andThenBoolSymbol
            , hookedSymbolDecl xorBoolSymbol
            , hookedSymbolDecl neBoolSymbol
            , hookedSymbolDecl eqBoolSymbol
            , hookedSymbolDecl impliesBoolSymbol
            , hookedSymbolDecl notBoolSymbol
            ]
        }

-- ** INT

intModuleName :: ModuleName
intModuleName = ModuleName "INT"

-- | Declare the @INT@ builtins.
intModule :: ParsedModule
intModule =
    Module
        { moduleName = intModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule boolModuleName
            , intSortDecl
            , -- comparison symbols
              hookedSymbolDecl gtIntSymbol
            , hookedSymbolDecl geIntSymbol
            , hookedSymbolDecl eqIntSymbol
            , hookedSymbolDecl leIntSymbol
            , hookedSymbolDecl ltIntSymbol
            , hookedSymbolDecl neIntSymbol
            , hookedSymbolDecl minIntSymbol
            , hookedSymbolDecl maxIntSymbol
            , hookedSymbolDecl addIntSymbol
            , hookedSymbolDecl subIntSymbol
            , hookedSymbolDecl mulIntSymbol
            , hookedSymbolDecl absIntSymbol
            , hookedSymbolDecl tdivIntSymbol
            , hookedSymbolDecl tmodIntSymbol
            , hookedSymbolDecl edivIntSymbol
            , hookedSymbolDecl emodIntSymbol
            , symbolDecl dummyIntSymbol
            , hookedSymbolDecl andIntSymbol
            , hookedSymbolDecl orIntSymbol
            , hookedSymbolDecl xorIntSymbol
            , hookedSymbolDecl notIntSymbol
            , hookedSymbolDecl shlIntSymbol
            , hookedSymbolDecl shrIntSymbol
            , hookedSymbolDecl powIntSymbol
            , hookedSymbolDecl powmodIntSymbol
            , hookedSymbolDecl log2IntSymbol
            ]
        }

-- ** KEQUAL

kEqualModuleName :: ModuleName
kEqualModuleName = ModuleName "KEQUAL"

-- | Declare the @KEQUAL@ builtins.
kEqualModule :: ParsedModule
kEqualModule =
    Module
        { moduleName = kEqualModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule boolModuleName
            , hookedSymbolDecl keqBoolSymbol
            , hookedSymbolDecl kneqBoolSymbol
            , hookedSymbolDecl kiteKSymbol
            , sortDecl kSort
            , sortDecl kItemSort
            , sortDecl idSort
            , symbolDecl kseqSymbol
            , symbolDecl dotkSymbol
            , injSymbolDecl
            ]
        }

subsortDecl :: Sort -> Sort -> ParsedSentence
subsortDecl subsort supersort =
    asSentence decl
  where
    decl :: ParsedSentenceAxiom
    decl =
        SentenceAxiom
            { sentenceAxiomParameters = [sortVariableR]
            , sentenceAxiomPattern =
                externalize
                    . mkExists x
                    $ mkEquals
                        sortR
                        (mkElemVar x)
                        (inj supersort (mkElemVar y))
            , sentenceAxiomAttributes =
                Attributes [subsortAttribute subsort supersort]
            }
    sortVariableR = SortVariable "R"
    sortR = SortVariableSort sortVariableR
    x = mkElementVariable "x" supersort
    y = mkElementVariable "y" subsort

injSymbolDecl :: ParsedSentence
injSymbolDecl =
    asSentence decl
  where
    decl :: ParsedSentenceSymbol
    decl =
        SentenceSymbol
            { sentenceSymbolSymbol =
                Symbol
                    { symbolConstructor =
                        let Internal.Symbol{symbolConstructor} =
                                injSymbol fromSort toSort
                         in symbolConstructor
                    , symbolParams = [fromSortVariable, toSortVariable]
                    }
            , sentenceSymbolSorts = [fromSort]
            , sentenceSymbolResultSort = toSort
            , sentenceSymbolAttributes =
                Attributes
                    [ sortInjectionAttribute
                    , injectiveAttribute
                    ]
            }
    fromSortVariable = SortVariable (testId "from")
    toSortVariable = SortVariable (testId "to")
    fromSort = SortVariableSort fromSortVariable
    toSort = SortVariableSort toSortVariable

-- ** LIST

listModuleName :: ModuleName
listModuleName = ModuleName "LIST"

-- | Declare the @LIST@ builtins.
listModule :: ParsedModule
listModule =
    Module
        { moduleName = listModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule intModuleName
            , listSortDecl
            , hookedSymbolDecl unitListSymbol
            , hookedSymbolDecl elementListSymbol
            , hookedSymbolDecl concatListSymbol
            , hookedSymbolDecl getListSymbol
            , hookedSymbolDecl updateListSymbol
            , hookedSymbolDecl inListSymbol
            , hookedSymbolDecl sizeListSymbol
            , hookedSymbolDecl makeListSymbol
            , hookedSymbolDecl updateAllListSymbol
            , -- A second builtin List sort, to confuse 'asPattern'.
              listSortDecl2
            , hookedSymbolDecl unitList2Symbol
            , hookedSymbolDecl elementList2Symbol
            , hookedSymbolDecl concatList2Symbol
            ]
        }

-- ** MAP

mapModuleName :: ModuleName
mapModuleName = ModuleName "MAP"

-- | Declare the @MAP@ builtins.
mapModule :: ParsedModule
mapModule =
    Module
        { moduleName = mapModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule boolModuleName
            , importParsedModule intModuleName
            , importParsedModule setModuleName
            , mapSortDecl
            , hookedSymbolDecl unitMapSymbol
            , hookedSymbolDecl elementMapSymbol
            , hookedSymbolDecl concatMapSymbol
            , hookedSymbolDecl lookupMapSymbol
            , hookedSymbolDecl lookupOrDefaultMapSymbol
            , hookedSymbolDecl updateMapSymbol
            , hookedSymbolDecl updateAllMapSymbol
            , hookedSymbolDecl inKeysMapSymbol
            , hookedSymbolDecl keysMapSymbol
            , hookedSymbolDecl keysListMapSymbol
            , hookedSymbolDecl removeMapSymbol
            , hookedSymbolDecl removeAllMapSymbol
            , hookedSymbolDecl sizeMapSymbol
            , hookedSymbolDecl valuesMapSymbol
            , hookedSymbolDecl inclusionMapSymbol
            ]
        }

-- ** PAIR

pairModuleName :: ModuleName
pairModuleName = ModuleName "PAIR"

-- | Declare the @Pair@ sort and constructors.
pairModule :: ParsedModule
pairModule =
    Module
        { moduleName = pairModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ pairSortDecl
            , pairSymbolDecl
            ]
        }

pairSymbolDecl :: ParsedSentence
pairSymbolDecl =
    asSentence decl
  where
    decl :: ParsedSentenceSymbol
    decl =
        SentenceSymbol
            { sentenceSymbolSymbol =
                Symbol
                    { symbolConstructor =
                        let Internal.Symbol{symbolConstructor} =
                                pairSymbol leftSort rightSort
                         in symbolConstructor
                    , symbolParams = [leftSortVariable, rightSortVariable]
                    }
            , sentenceSymbolSorts = [leftSort, rightSort]
            , sentenceSymbolResultSort = pairSort leftSort rightSort
            , sentenceSymbolAttributes =
                Attributes
                    [ constructorAttribute
                    , injectiveAttribute
                    , totalAttribute
                    ]
            }
    leftSortVariable = SortVariable (testId "left")
    rightSortVariable = SortVariable (testId "right")
    leftSort = SortVariableSort leftSortVariable
    rightSort = SortVariableSort rightSortVariable

-- ** SET

setModuleName :: ModuleName
setModuleName = ModuleName "SET"

-- | Declare the @SET@ builtins.
setModule :: ParsedModule
setModule =
    Module
        { moduleName = setModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule intModuleName
            , importParsedModule boolModuleName
            , importParsedModule listModuleName
            , setSortDecl
            , testSortDecl
            , hookedSymbolDecl unitSetSymbol
            , hookedSymbolDecl elementSetSymbol
            , hookedSymbolDecl concatSetSymbol
            , hookedSymbolDecl inSetSymbol
            , hookedSymbolDecl differenceSetSymbol
            , hookedSymbolDecl toListSetSymbol
            , hookedSymbolDecl sizeSetSymbol
            , hookedSymbolDecl intersectionSetSymbol
            , hookedSymbolDecl list2setSetSymbol
            , hookedSymbolDecl inclusionSetSymbol
            ]
        }

-- ** STRING

stringModuleName :: ModuleName
stringModuleName = ModuleName "STRING"

-- | Declare the @STRING@ builtins.
stringModule :: ParsedModule
stringModule =
    Module
        { moduleName = stringModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule boolModuleName
            , importParsedModule intModuleName
            , stringSortDecl
            , userTokenSortDecl
            , hookedSymbolDecl eqStringSymbol
            , hookedSymbolDecl ltStringSymbol
            , hookedSymbolDecl concatStringSymbol
            , hookedSymbolDecl substrStringSymbol
            , hookedSymbolDecl lengthStringSymbol
            , hookedSymbolDecl chrStringSymbol
            , hookedSymbolDecl ordStringSymbol
            , hookedSymbolDecl findStringSymbol
            , hookedSymbolDecl string2BaseStringSymbol
            , hookedSymbolDecl base2StringStringSymbol
            , hookedSymbolDecl string2IntStringSymbol
            , hookedSymbolDecl int2StringStringSymbol
            , hookedSymbolDecl token2StringStringSymbol
            , hookedSymbolDecl string2TokenStringSymbol
            ]
        }

-- ** IO

ioModuleName :: ModuleName
ioModuleName = ModuleName "IO"

ioModule :: ParsedModule
ioModule =
    Module
        { moduleName = ioModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule kEqualModuleName
            , importParsedModule stringModuleName
            , hookedSymbolDecl logStringSymbol
            ]
        }

-- ** BYTES

bytesModuleName :: ModuleName
bytesModuleName = ModuleName "BYTES"

-- | Declare the @BYTES@ builtins.
bytesModule :: ParsedModule
bytesModule =
    Module
        { moduleName = bytesModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule stringModuleName
            , importParsedModule intModuleName
            , bytesSortDecl
            , hookedSymbolDecl bytes2stringBytesSymbol
            , hookedSymbolDecl string2bytesBytesSymbol
            , hookedSymbolDecl decodeBytesBytesSymbol
            , hookedSymbolDecl encodeBytesBytesSymbol
            , hookedSymbolDecl updateBytesSymbol
            , hookedSymbolDecl getBytesSymbol
            , hookedSymbolDecl substrBytesSymbol
            , hookedSymbolDecl replaceAtBytesSymbol
            , hookedSymbolDecl padRightBytesSymbol
            , hookedSymbolDecl padLeftBytesSymbol
            , hookedSymbolDecl reverseBytesSymbol
            , hookedSymbolDecl lengthBytesSymbol
            , hookedSymbolDecl concatBytesSymbol
            , hookedSymbolDecl int2bytesSymbol
            , hookedSymbolDecl bytes2intSymbol
            , endiannessSortDecl
            , symbolDecl littleEndianBytesSymbol
            , symbolDecl bigEndianBytesSymbol
            , signednessSortDecl
            , symbolDecl signedBytesSymbol
            , symbolDecl unsignedBytesSymbol
            ]
        }

-- ** KRYPTO

kryptoModuleName :: ModuleName
kryptoModuleName = ModuleName "KRYPTO"

-- | Declare the @KRYPTO@ builtins.
kryptoModule :: ParsedModule
kryptoModule =
    Module
        { moduleName = kryptoModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule bytesModuleName
            , importParsedModule stringModuleName
            , importParsedModule intModuleName
            , importParsedModule listModuleName
            , hookedSymbolDecl ecdsaRecoverSymbol
            , hookedSymbolDecl keccak256Symbol
            , hookedSymbolDecl sha256Symbol
            , hookedSymbolDecl sha3256Symbol
            , hookedSymbolDecl ripemd160Symbol
            ]
        }

-- ** TEST

testModuleName :: ModuleName
testModuleName = ModuleName "TEST"

testModule :: ParsedModule
testModule =
    Module
        { moduleName = testModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ importParsedModule boolModuleName
            , importParsedModule intModuleName
            , importParsedModule kEqualModuleName
            , importParsedModule listModuleName
            , importParsedModule mapModuleName
            , importParsedModule pairModuleName
            , importParsedModule setModuleName
            , importParsedModule stringModuleName
            , importParsedModule bytesModuleName
            , importParsedModule kryptoModuleName
            , importParsedModule ioModuleName
            , subsortDecl boolSort kItemSort
            , subsortDecl intSort kItemSort
            , subsortDecl idSort kItemSort
            , subsortDecl kItemSort kSort
            ]
        }

testModuleWithTwoClaims :: ParsedModule
testModuleWithTwoClaims =
    Module
        { moduleName = testModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ SentenceClaimSentence . SentenceClaim $
                ( SentenceAxiom
                    { sentenceAxiomParameters = [SortVariable (testId "sv1")]
                    , sentenceAxiomPattern =
                        externalize (mkStringLiteral "a")
                    , sentenceAxiomAttributes =
                        Attributes
                            [ embedParsedPattern $
                                PatternF.StringLiteralF $
                                    Const (StringLiteral "b")
                            ]
                    } ::
                    ParsedSentenceAxiom
                )
            , SentenceClaimSentence . SentenceClaim $
                ( SentenceAxiom
                    { sentenceAxiomParameters = [SortVariable (testId "sv2")]
                    , sentenceAxiomPattern =
                        externalize (mkStringLiteral "c")
                    , sentenceAxiomAttributes =
                        Attributes
                            [ embedParsedPattern $
                                PatternF.StringLiteralF $
                                    Const (StringLiteral "b")
                            ]
                    } ::
                    ParsedSentenceAxiom
                )
            ]
        }

-- -------------------------------------------------------------

-- * Definition

testDefinition :: ParsedDefinition
testDefinition =
    Definition
        { definitionAttributes = Attributes []
        , definitionModules =
            [ boolModule
            , intModule
            , kEqualModule
            , listModule
            , mapModule
            , pairModule
            , setModule
            , stringModule
            , ioModule
            , bytesModule
            , kryptoModule
            , testModule
            ]
        }
