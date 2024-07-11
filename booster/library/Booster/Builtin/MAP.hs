{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause

Built-in functions (hooks) in the MAP namespace, as described in
[docs/hooks.md](https://github.com/runtimeverification/haskell-backend/blob/master/docs/hooks.md).

* Only selected hooks are implemented.
* Depends on the built-in types List, Bool and Int.
-}
module Booster.Builtin.MAP (
    builtinsMAP,
) where

import Control.Monad.Trans.Except (throwE)
import Data.ByteString.Char8 (ByteString)
import Data.List (findIndex, partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Booster.Builtin.BOOL
import Booster.Builtin.Base
import Booster.Builtin.INT
import Booster.Builtin.LIST
import Booster.Definition.Attributes.Base (KMapDefinition (..))
import Booster.Pattern.Base

builtinsMAP :: Map ByteString BuiltinFunction
builtinsMAP =
    Map.mapKeys ("MAP." <>) $
        Map.fromList
            [ "update" ~~> mapUpdateHook
            , "updateAll" ~~> mapUpdateAllHook
            , "remove" ~~> mapRemoveHook
            , -- removeAll: requires a Set argument
              "size" ~~> mapSizeHook
            , "lookup" ~~> mapLookupHook
            , "lookupOrDefault" ~~> mapLookupOrDefaultHook
            , "in_keys" ~~> mapInKeysHook
            , -- keys: requires internal Set construction
              "keys_list" ~~> mapKeysListHook
            , "values" ~~> mapValuesHook
            , "inclusion" ~~> mapInclusionHook
            ]

mapUpdateHook :: BuiltinFunction
mapUpdateHook args
    | [KMap def pairs mbRest, key, newValue] <- args = do
        case findIndex ((== key) . fst) pairs of
            Just idx ->
                -- key was found (syntactically), update pairs list
                let newPairs = take idx pairs <> ((key, newValue) : drop (idx + 1) pairs)
                 in pure $ Just $ KMap def newPairs mbRest
            Nothing -- key could be in unevaluated or opaque part
                | Just _ <- mbRest ->
                    pure Nothing -- have opaque part, no result
                | any (not . isConstructorLike_ . fst) pairs ->
                    pure Nothing -- have unevaluated keys, no result
                | not $ isConstructorLike_ key ->
                    pure Nothing -- unevaluated update key, no result
                | otherwise -> -- key certain to be absent, no rest: add pair
                    pure $ Just $ KMap def ((key, newValue) : pairs) Nothing
    | [_other, _, _] <- args =
        pure Nothing -- not an internalised map, maybe a function call
    | otherwise =
        arityError "MAP.update" 3 args

mapUpdateAllHook :: BuiltinFunction
mapUpdateAllHook [KMap def1 _ _, KMap def2 _ _]
    | def1 /= def2 =
        throwE $
            "MAP.updateAll: incompatible maps "
                <> Text.pack (show (def1.mapSortName, def2.mapSortName))
mapUpdateAllHook [original, KMap _ [] Nothing] =
    -- updates map is empty, result is original map
    pure $ Just original
mapUpdateAllHook [KMap _ [] Nothing, updates] =
    -- original map is empty, result is updates map
    pure $ Just updates
mapUpdateAllHook [original@(KMap _ pairs1 rest1@(Just _)), KMap _ pairs2 rest2]
    | pairs1 == pairs2
    , rest1 == rest2 -- identical maps, result is original
        =
        pure $ Just original
    | otherwise -- indeterminate part in original map, leave unevaluated
        =
        pure Nothing
mapUpdateAllHook [KMap def pairs1 Nothing, KMap _ pairs2 mbRest2]
    -- performing the update requires all keys to be fully evaluated
    -- (constructor-like) or syntactically equal.
    | Set.null origKeys -- all keys in the original map were updated (syntactically)
        =
        pure $ Just $ KMap def updated mbRest2
    | Set.null updateKeys
    , Nothing <- mbRest2 -- all update keys were (syntactically) present
        =
        pure $ Just $ KMap def updated Nothing
    | all isConstructorLike_ (updateKeys <> origKeys)
    , Nothing <- mbRest2 -- all untouched or added keys are fully evaluated
        =
        pure $ Just $ KMap def updated Nothing
    | otherwise -- uncertain whether all keys updated, leave unevaluated
        =
        pure Nothing
  where
    orig = Map.fromList pairs1
    update = Map.fromList pairs2
    updated = Map.assocs $ Map.unionWith (\_ u -> u) orig update
    origKeys = Set.difference (Map.keysSet orig) (Map.keysSet update)
    updateKeys = Set.difference (Map.keysSet update) (Map.keysSet orig)
mapUpdateAllHook [_, _] =
    pure Nothing -- at least one argument not an internalised map, leave unevaluated
mapUpdateAllHook args =
    arityError "MAP.update" 2 args

mapRemoveHook :: BuiltinFunction
mapRemoveHook args
    | [m@(KMap def pairs mbRest), key] <- args = do
        case findIndex ((== key) . fst) pairs of
            Just idx ->
                -- key was found (syntactically), remove it
                let newPairs = take idx pairs <> drop (idx + 1) pairs
                 in pure $ Just $ KMap def newPairs mbRest
            Nothing -- key could be in unevaluated or opaque part
                | Just _ <- mbRest ->
                    pure Nothing -- have opaque part, no result
                | any (not . isConstructorLike_ . fst) pairs ->
                    pure Nothing -- have unevaluated keys, no result
                | not $ isConstructorLike_ key ->
                    pure Nothing -- remove key unevaluated, no result
                | otherwise -> -- key certain to be absent, no rest: map unchanged
                    pure $ Just m
    | [_other, _] <- args =
        pure Nothing -- not an internalised map, maybe a function call
    | otherwise =
        arityError "MAP.remove" 2 args

mapSizeHook :: BuiltinFunction
mapSizeHook args
    | [KMap _ pairs Nothing] <- args =
        -- no opaque part, size is association count
        pure $ Just $ intTerm (fromIntegral $ length pairs)
    | [_other] <- args =
        pure Nothing -- not an internalised map, maybe a function call
    | otherwise =
        arityError "MAP.lookup" 1 args

mapLookupHook :: BuiltinFunction
mapLookupHook args
    | [KMap _ pairs _mbRest, key] <- args =
        -- if the key is not found, return Nothing (no result),
        -- regardless of whether the key _could_ still be there.
        pure $ lookup key pairs
    | [_other, _] <- args =
        pure Nothing -- not an internalised map, maybe a function call
    | otherwise =
        arityError "MAP.lookup" 2 args

mapLookupOrDefaultHook :: BuiltinFunction
mapLookupOrDefaultHook args
    | [KMap _ pairs mbRest, key, defaultValue] <- args = do
        case lookup key pairs of
            Just value ->
                -- key was found, simply return
                pure $ Just value
            Nothing -- key could be in unevaluated or opaque part
                | Just _ <- mbRest ->
                    pure Nothing -- have opaque part, no result
                | any (not . isConstructorLike_ . fst) pairs ->
                    pure Nothing -- have unevaluated keys, no result
                | not $ isConstructorLike_ key ->
                    pure Nothing -- lookup key unevaluated, no result
                | otherwise -> -- certain that the key is not in the map
                    pure $ Just defaultValue
    | [_other, _, _] <- args =
        pure Nothing -- not an internalised map, maybe a function call
    | otherwise =
        arityError "MAP.lookupOrDefault" 3 args

mapInKeysHook :: BuiltinFunction
mapInKeysHook args
    | [key, KMap _ pairs mbRest] <- args = do
        -- only consider evaluated keys, return Nothing if any unevaluated ones are present
        let (eval'edKeys, uneval'edKeys) =
                partition isConstructorLike_ (map fst pairs)
        case (key `elem` eval'edKeys, key `elem` uneval'edKeys) of
            (True, _) ->
                -- constructor-like (evaluated) key is present
                pure $ Just $ boolTerm True
            (False, True) ->
                -- syntactically-equal unevaluated key is present
                pure $ Just $ boolTerm True
            (False, False)
                | Nothing <- mbRest -- no opaque rest
                , isConstructorLike_ key -- key to search is evaluated
                , null uneval'edKeys -> -- no keys unevaluated
                    pure $ Just $ boolTerm False
                | otherwise -> -- key could be present once evaluated
                    pure Nothing
    | [_, _other] <- args = do
        -- other `shouldHaveSort` "SortMap"
        pure Nothing -- not an internalised map, maybe a function call
    | otherwise =
        arityError "MAP.in_keys" 2 args

mapKeysListHook :: BuiltinFunction
mapKeysListHook = \case
    [KMap _ pairs Nothing] ->
        -- known keys only, return as list
        pure $ Just $ KList kItemListDef (map fst pairs) Nothing
    [KMap _ _ (Just _)] ->
        -- indeterminate part
        pure Nothing
    [_arg] ->
        -- unevaluated
        pure Nothing
    args ->
        arityError "MAP.keys_list" 1 args

mapValuesHook :: BuiltinFunction
mapValuesHook = \case
    [KMap _ pairs Nothing] ->
        -- known values only, return as list
        pure $ Just $ KList kItemListDef (map snd pairs) Nothing
    [KMap _ _ (Just _)] ->
        -- indeterminate part
        pure Nothing
    [_arg] ->
        -- unevaluated
        pure Nothing
    args ->
        arityError "MAP.values" 1 args

mapInclusionHook :: BuiltinFunction
mapInclusionHook = \case
    [KMap d1 pairs1 mbRest1, KMap d2 pairs2 mbRest2]
        | d1 /= d2 -> -- different kinds of map
            pure Nothing
        | pairs1 == pairs2 -- syntactically identical maps
        , mbRest1 == mbRest2 ->
            pure $ Just $ boolTerm True
    [KMap _ pairs1 Nothing, KMap _ pairs2 mbRest2]
        -- fully concrete maps
        | keySet pairs1 `Set.isSubsetOf` keySet pairs2 ->
            pure $ Just $ boolTerm True
        | all isConstructorLike_ (keySet pairs1)
        , all isConstructorLike_ (keySet pairs2)
        , Nothing <- mbRest2 -> -- fully-known keys, certain to not be subset
            pure $ Just $ boolTerm False
        | otherwise ->
            pure Nothing -- unevaluated keys present, indeterminate
    [KMap _ _ (Just _), KMap _ _ _] ->
        pure Nothing -- indeterminate part cannot be checked
    [KMap def [] Nothing, term] -> do
        -- empty map included in any map of correct sort
        term `shouldHaveSort` def.mapSortName
        pure $ Just $ boolTerm True
    [t1, t2]
        | t1 == t2 -> -- identical arguments
            pure $ Just $ boolTerm True
        | otherwise -> -- unevaluated map
            pure Nothing
    args ->
        arityError "MAP.inclusion" 2 args
  where
    keySet = Set.fromList . map fst
