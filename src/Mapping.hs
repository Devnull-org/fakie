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
      liftIO $
        putStrLn "No config file detected! We expect to see configuration inside of .fakie.json file."
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

-- Go through each mapping in the configuration and map user keys to the api ones.
assignUserKeys :: FakieItem -> Value -> MappingContext
assignUserKeys FakieItem {..} apiValue =
  let mapping =
        -- initialize the context with empty error string and one empty Object
        flip execState (MappingContext [] (Object empty)) $
          mapM
            (\FakieMap {..} ->
              -- Try to find the api key inside of api results
              case findInPath fakieMapTheirkey apiValue Nothing of
                Left _ -> noteError fakieMapTheirkey
                -- we found the key, map it to our user json
                Right foundVal -> do
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

-- | Key not found but we want custom error message
noteError :: Text -> State MappingContext ()
noteError textErr = do
  modify'
    (\mc ->
       MappingContext
         (textErr : mappingContextPossibleErrors mc)
         (mappingContextValue mc)
    )

-- | Find the path that uses dot notation to drill into the data
findInPath :: Text -> Value -> Maybe Int -> Either Text Value
findInPath fakieMapTheirkey ourMappedValue mArrIndex = do
  let pathToSearch = T.splitOn "." fakieMapTheirkey
  case pathToSearch of
    [] -> Left fakieMapTheirkey
    path -> findPathRecusive ourMappedValue path mArrIndex

-- | Recurse using the dotted keys with posibility of failure
findPathRecusive :: Value -> [Text] -> Maybe Int -> Either Text Value
findPathRecusive currentValue currentPath mArrIndex
  | isNothing (ER.headMay currentPath) = Right currentValue
  | otherwise =
      case ER.headMay currentPath of
        Nothing -> Left "empty key"
        Just pathSegment -> do
          val <- findValueKey pathSegment currentValue mArrIndex
          findPathRecusive val (tail currentPath) mArrIndex

-- | Find the key inside of json Object or Array.
-- Optionally we can search specific array index
findValueKey :: Text -> Value -> Maybe arrayIndex -> Either Text Value
findValueKey k (Object obj) _ =
  if | hasSpecialKeys k ->
       case k of
         "Object" -> Right (Object obj)
         _        -> Left k
     | otherwise ->
         case lookup k obj of
           Nothing -> Left (indexError k)
           Just foundObj -> Right foundObj
findValueKey k (Array arr) arrayIndex =
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
    go k' arr' ind = do
      case arr' V.!? ind of
        Nothing -> Left (indexError ind)
        Just arrayValue ->
          case findValueKey k' arrayValue arrayIndex of
            Left _  -> go k' arr' (ind + 1)
            Right val -> Right val
findValueKey k _ _ = Left k
