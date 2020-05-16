{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Mapping where

import           Common
import qualified Control.Error          as ER
import           Control.Exception.Safe (MonadThrow, SomeException (..), throwM,
                                         tryAny)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.CaseInsensitive   as CI
import           Data.HashMap.Strict    (delete, empty, insert, lookup)
import           Data.List              (tail)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Vector            as V
import           Network.HTTP.Client    (Request (..), parseUrlThrow)
import           Network.HTTP.Simple
import           System.Directory       (getCurrentDirectory, listDirectory)
import           System.FilePath        ((</>))
import           Types

readFakieConfig
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader FakieEnv m)
  => m (Either SomeException Fakie)
readFakieConfig =
  ER.runExceptT $ do
    FakieEnv {..} <- lift ask
    cwd <- ER.ExceptT $ liftIO (tryAny getCurrentDirectory)
    fileList <- ER.ExceptT $ liftIO (tryAny $ listDirectory cwd)
    if configFileName `notElem` fileList
      then do
      liftIO $ fakieEnvLog "No config file detected! We expect to see configuration inside of .fakie.json file."
      throwM (FakieException "No config file detected! Check the log file for errors.")
      else do
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile $ cwd </> configFileName)
        ER.hoistEither $
          ER.fmapL (SomeException . FakieException) (eitherDecode fileContents :: Either String [FakieItem])

callApi
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader FakieEnv m
     )
  => FakieItem
  -> m Value
callApi fItem@FakieItem {..} = do
  FakieEnv {..} <- ask
  if | fakieEnvTesting -> liftIO $ do
        ebody <- BSL.readFile "/home/v0d1ch/code/fakie/src/json/Posts.json"
        case eitherDecode ebody :: Either String Value of
          Left err ->
            throwM (FakieException $ show err)
          Right body -> return body
     | otherwise -> liftIO $ do
         erequest <-
           tryAny $
             parseUrlThrow (show fakieItemMethod <> " " <> T.unpack fakieItemUrl)
         case erequest of
           Left err -> throwM (FakieException (show err))
           Right request'' -> do
             let request' =
                   setRequestQueryString (constructQueryParams fItem) $
                   setUpRequestHeaders request'' fItem
                 request = setUpRequestBody request' fItem
             response <- httpJSONEither request
             case getResponseBody response :: Either JSONException Value of
               Left err -> throwM (FakieException (show err))
               Right rBody -> do
                 when (getResponseStatusCode response /= 200) $
                   throwM (FakieException "Received non 200 status code!")
                 return rBody

setUpRequestBody :: Request -> FakieItem -> Request
setUpRequestBody r FakieItem {..} =
  case fakieItemBody of
    Nothing   -> r
    Just body -> setRequestBodyJSON body r

setUpRequestHeaders :: Request -> FakieItem -> Request
setUpRequestHeaders r FakieItem {..} =
  let formattedHeaders =
        (\FakieHeader {..} ->
          (CI.mk $ encodeUtf8 fakieHeaderName, encodeUtf8 fakieHeaderValue)
        ) <$> fakieItemHeaders
  in setRequestHeaders formattedHeaders r

-- | Construct the 'Query' datatype from 'FakieQueryParams'. Basically a key value pairs.
constructQueryParams :: FakieItem -> Query
constructQueryParams FakieItem {..} =
  (\FakieQueryParam {..} ->
    (encodeUtf8 fakieQueryParamName, Just (encodeUtf8 fakieQueryParamValue))
  ) <$> fakieItemQueryParams

-- | Find the key inside of json Object or Array.
-- Optionally we can search specific array index
findValueKey :: Text -> Value -> Maybe arrayIndex -> Maybe Value
findValueKey k (Object obj) _ =
  if | hasSpecialKeys k ->
       case k of
         "Object" -> Just (Object obj)
         _        -> Nothing
     | otherwise -> lookup k obj
findValueKey k (Array arr) arrayIndex =
  if | hasSpecialKeys k ->
       case k of
         "Array" -> Just (Array arr)
         -- if there is a nth key try to find array value at that index
         _ -> (V.!?) arr =<< getNth k
     | otherwise -> go k arr 0
  where
    go :: Text -> V.Vector Value -> Int -> Maybe Value
    go k' arr' ind = do
      arrayValue <- arr' V.!? ind
      case findValueKey k' arrayValue arrayIndex of
        Nothing  -> go k' arr' (ind + 1)
        Just val -> Just val
findValueKey _ _ _ = Nothing

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
getNth :: Text -> Maybe Int
getNth key = do
  let nthElements = T.breakOn "-" key
  if fst nthElements == "nth"
    then readMaybe . T.unpack $ T.replace "-" "" (snd nthElements)
    else Nothing

-- | Given a key, new value and already existing value put the new value under a provided key
-- into a existing Object or Array.
-- If the specified key already exists just return that value instead.
createValueKey :: Text -> Value -> Value -> Value
createValueKey key newValue userValue =
  case findValueKey key userValue Nothing of
    -- ok it is safe to insert new value
    Nothing ->
      case userValue of
        Object obj ->
          let obj' = insert key newValue obj
          in Object obj'
        Array arr ->
          let arr' = V.snoc arr newValue
          in Array arr'
        _ -> userValue
    -- value with this key is already there, just return it
    Just _ -> userValue

-- | Removes a value with specified keys from json Objects
removeValueKey :: Text -> Value -> Value
removeValueKey removeKey (Object o) = Object (delete removeKey o)
removeValueKey removeKey _ =
  error $ "Trying to remove key " <> T.unpack removeKey <> " from value that does not have keys"

-- | Special keys are used to mark that some json portions that don't have keys assigned to them.
-- Later on we can choose to keep this data with shouldKeep flag.
-- We can also drill further into this data using '.' (dot) similar to javascript objects
-- or some special syntax like 'nth Number' TODO: implement nth functionality for Arrays

-- Go through each mapping in the configuration and map user keys to the api ones.
assignUserKeys :: FakieItem -> Value -> MappingContext
assignUserKeys FakieItem {..} apiValue =
  let mapping =
        -- initialize the context with empty error string and one empty Object
        flip execState (MappingContext "" (Object empty)) $
          mapM
            (\fm@FakieMap {..} ->
               if | "." `T.isInfixOf` fakieMapTheirkey -> mapDotKeys fm
                  | otherwise ->
                      -- Try to find the api key inside of api results
                      case findValueKey fakieMapTheirkey apiValue Nothing of
                        Nothing -> noteKeyError fakieMapTheirkey
                        -- we found the key, map it to our user json
                        Just foundVal -> do
                          context  <- get
                          let newValue =
                                createValueKey fakieMapOurkey foundVal (mappingContextValue context)
                          modify'
                            (\mc ->
                               MappingContext (mappingContextPossibleErrors mc) newValue
                            )
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
                    (mappingContextPossibleErrors mc)
                    (removeValueKey key (mappingContextValue mc))
                )
            ) keysToRemove
  in
    adjustedMapping

-- | Map the keys containing "." (dot) accessors
mapDotKeys :: FakieMap -> State MappingContext ()
mapDotKeys fm@FakieMap {..} = do
  let path = T.splitOn "." fakieMapTheirkey
  case path of
    []            -> noteKeyError fakieMapTheirkey
    pathToDrillIn -> findDottedPath fm pathToDrillIn

-- | Key was not found, alert the user about it
noteKeyError :: Text -> State MappingContext ()
noteKeyError notFoundKey =
  modify'
    (\mc ->
       MappingContext
         (mappingContextPossibleErrors mc <> "Key " <> notFoundKey <> " not found!")
         (mappingContextValue mc)
    )

-- | Find the path that uses dot notation to drill into the data
findDottedPath :: FakieMap -> [Text] -> State MappingContext ()
findDottedPath FakieMap {..} path = do
  context <- get
  let ourMappedValue = mappingContextValue context
      eFoundValue = findPathRecusive ourMappedValue path
  case eFoundValue of
    Left keyNotFound -> noteKeyError keyNotFound
    Right foundVal -> do
      let newValue =
            createValueKey fakieMapOurkey foundVal ourMappedValue
      modify'
        (\mc ->
           MappingContext (mappingContextPossibleErrors mc) newValue
        )

-- | Recurse using the dotted keys with posibility of failure
findPathRecusive :: Value -> [Text] -> Either Text Value
findPathRecusive currentValue currentPath
  | isNothing (ER.headMay currentPath) = Right currentValue
  | otherwise =
      case ER.headMay currentPath of
        Nothing -> Left "empty key"
        Just pathSegment ->
          case findValueKey pathSegment currentValue Nothing of
            Nothing  -> Left pathSegment
            Just val -> findPathRecusive val (tail currentPath)
