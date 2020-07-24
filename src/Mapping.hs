{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Mapping
  ( assignUserKeys
  , findValueKey
  , findInPath
  ) where

import           Common
import qualified Control.Error            as ER
import           Data.Aeson
import           Data.HashMap.Strict      (delete, empty, insert, lookup)
import           Data.List                (tail)
import           Data.Maybe               (isNothing)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Types

-- Go through each mapping in the configuration and map user keys to the api ones.
assignUserKeys :: FakieItem -> Value -> MappingContext
assignUserKeys FakieItem {..} apiValue =
  let
    emptyMappingContext =
      MappingContext
        { mappingContextFailures = []
        , mappingContextErrors = []
        , mappingContextValue = Object empty
        }
    mapping =
      -- initialize the context with empty error string and one empty Object
      flip execState emptyMappingContext $
        mapM
          (\FakieMap {..} -> do
            context  <- get
            let
              -- Try to find the api key inside of api results
              eApiResults = findInPath fakieMapTheirkey apiValue Nothing
              -- try to look in our current mapped value so far for the key.
              -- This is the case where we want to assign some value to our key so
              -- we can look it up later on.
              eAllreadyMappedLookup =
                findInPath fakieMapTheirkey (mappingContextValue context) Nothing
              -- grab either value from api results or already mapped ones
              eFoundVal = ER.either (const eAllreadyMappedLookup) Right eApiResults
            case eFoundVal of
              Left _ -> noteError fakieMapTheirkey
              -- we found the key, map it to our user json
              Right foundVal -> addToMap fakieMapOurkey foundVal context
          ) fakieItemMapping
    -- we filter our the keys we should remove from final json
    keysToRemove =
      fakieMapOurkey <$>
      filter
        (\FakieMap {..} ->
           fakieMapShouldKeep == Just False
        ) fakieItemMapping
      -- when we finish with the mapping go through the results and remove the keys
      -- marked for removal
    adjustedMapping =
      flip execState mapping $
        mapM
          (\key ->
            modify'
              (\mc ->
                 MappingContext
                   { mappingContextFailures = []
                   , mappingContextErrors = mappingContextErrors mc
                   , mappingContextValue = removeValueKey key (mappingContextValue mc)
                   }
              )
          ) keysToRemove
  in
    adjustedMapping

addToMap
  :: Text
  -> Value
  -> MappingContext
  -> State MappingContext ()
addToMap ourKey foundVal currentContext = do
  let newValue =
        createValueKey ourKey foundVal (mappingContextValue currentContext)
  modify'
    (\mc ->
       MappingContext
         { mappingContextFailures = []
         , mappingContextErrors = mappingContextErrors mc
         , mappingContextValue = newValue
         }
    )

--------------------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------------------

indexError :: Show a => a -> Text
indexError i = "Could not find index " <> T.pack (show i)

hasSpecialKeys :: Text -> Bool
hasSpecialKeys key
  | key `elem` specialKeys = True
  | "nth" == fst (T.breakOn "-" key) = True
  | otherwise = False
  where
    specialKeys :: [Text]
    specialKeys = ["Array", "Object"]

-- | Tries to find special key 'nth-' that denotes we want to extract specific array key.
-- If nth key exists then we want to try to extract the actuall array index to look for data.
getNth :: Text -> Either Text Int
getNth key = do
  let nthElements = T.breakOn "-" key
  if fst nthElements == "nth"
    then maybe (Left key) Right (readMaybe . T.unpack $ T.replace "-" "" (snd nthElements))
    else Left key

-- | Given a key, new value and already existing value put the new value under a provided key
-- into a existing Object or Array.
-- If the specified key already exists just return that value instead.
createValueKey :: Text -> Value -> Value -> Value
createValueKey key newValue userValue =
  case findInPath key userValue Nothing of
    -- We didn't find the value - it is safe to insert new value
    Left _ ->
      case userValue of
        Object obj ->
          let obj' = insert key newValue obj
          in Object obj'
        Array arr ->
          let arr' = V.snoc arr newValue
          in Array arr'
        _ -> userValue
    -- value with this key is already there, just return it
    Right _ -> userValue

-- | Removes a value with specified keys from json Objects
removeValueKey :: Text -> Value -> Value
removeValueKey removeKey (Object o) = Object (delete removeKey o)
removeValueKey removeKey _ =
  error $
    "Trying to remove key " <>
    T.unpack removeKey <>
    " from value that does not have keys"

-- | Special keys are used to mark that some json portions that don't have keys assigned to them.
-- Later on we can choose to keep this data with shouldKeep flag.
-- We can also drill further into this data using '.' (dot) similar to javascript objects
-- or some special syntax like 'nth Number'

-- | Key not found but we want custom error message
noteError :: Text -> State MappingContext ()
noteError textErr =
  modify'
    (\mc ->
       MappingContext
         { mappingContextFailures = []
         , mappingContextErrors = textErr : mappingContextErrors mc
         , mappingContextValue = mappingContextValue mc
         }
    )

-- | Find the path that uses dot notation to drill into the data
findInPath :: Text -> Value -> Maybe Int -> Either Text Value
findInPath fakieMapTheirkey ourMappedValue mArrIndex = do
  let pathToSearch = T.splitOn "." fakieMapTheirkey
  case pathToSearch of
    []   -> Left fakieMapTheirkey
    path -> findPathRecusive ourMappedValue path mArrIndex

-- | Recurse using the dotted keys with posibility of failure
findPathRecusive :: Value -> [Text] -> Maybe Int -> Either Text Value
findPathRecusive currentValue currentPath mArrIndex
  | isNothing (ER.headMay currentPath) = Right currentValue
  | otherwise =
      case ER.headMay currentPath of
        Nothing -> Left "empty key"
        Just pathSegment -> do
          val <- findValueKey pathSegment mArrIndex currentValue
          findPathRecusive val (tail currentPath) mArrIndex

-- | Find the key inside of json Object or Array.
-- Optionally we can search specific array index
findValueKey :: Text -> Maybe arrayIndex -> Value -> Either Text Value
findValueKey k _ (Object obj)  =
  if | hasSpecialKeys k ->
       case k of
         "Object" -> Right (Object obj)
         _        -> Left k
     | otherwise ->
         case lookup k obj of
           Nothing       -> Left (indexError k)
           Just foundObj -> Right foundObj
findValueKey k arrayIndex (Array arr) =
  if | hasSpecialKeys k ->
       case k of
         "Array" -> Right (Array arr)
         -- if there is a nth key try to find array value at that index
         _ ->
           case getNth k of
             Left err -> Left err
             Right arrInd ->
               maybe (Left $ indexError arrInd) Right (arr V.!? arrInd)
     | otherwise -> go k arr 0
  where
    go :: Text -> V.Vector Value -> Int -> Either Text Value
    go k' arr' ind =
      case arr' V.!? ind of
        Nothing -> Left (indexError ind)
        Just arrayValue ->
          case findValueKey k' arrayIndex arrayValue  of
            Left _    -> go k' arr' (ind + 1)
            Right val -> Right val
findValueKey k _ _ = Left k
