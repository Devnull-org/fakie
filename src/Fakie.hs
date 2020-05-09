{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Fakie where

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
import           Data.List              (tail, take)
import           Data.Maybe             (isNothing)
import           Data.Monoid            (mconcat)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Vector            as V
import           Network.HTTP.Client    (Request (..), parseUrlThrow)
import           Network.HTTP.Simple
import           System.Directory       (getCurrentDirectory, listDirectory)
import           System.FilePath        ((</>))
import           Types

readFakieConfig :: (MonadIO m, MonadThrow m) => m (Either SomeException Fakie)
readFakieConfig =
  ER.runExceptT $ do
    cwd <- ER.ExceptT $ liftIO (tryAny getCurrentDirectory)
    fileList <- ER.ExceptT $ liftIO (tryAny $ listDirectory cwd)
    if configFileName `notElem` fileList
      then throwM (FakieException "No config file detected")
      else do
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile $ cwd </> configFileName)
        ER.hoistEither $
          ER.fmapL (SomeException . FakieException) (eitherDecode fileContents :: Either String [FakieItem])

callApi :: (MonadIO m, MonadThrow m, MonadReader FakieEnv m) => FakieItem -> m Value
callApi fItem@FakieItem {..} = do
  FakieEnv {..} <- ask
  liftIO $ do
    fakieEnvLog "calling the apis"
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
    (encodeUtf8 fakieQueryParamName,  Just (encodeUtf8 fakieQueryParamValue))
  ) <$> fakieItemQueryParams

-- | Find the key inside of json Object or Array of Objects
findValueKey :: Text -> Value -> Maybe Value
findValueKey k (Object obj) = lookup k obj
findValueKey k (Array arr) =
  go k arr 0
  where
    go k' arr' ind
      | isNothing (arr' V.!? ind) = Nothing
      | otherwise =
          case arr' V.!? ind of
            Nothing -> Nothing
            Just v ->
              case findValueKey k' v of
                Nothing  -> go k' arr' (ind + 1)
                Just val -> Just val
findValueKey _ _ = Nothing

-- | Given a key, new value and already existing value put the new value under a provided key
-- into a existing Object or Array.
-- If the specified key already exists just return that value instead.
createValueKey :: Text -> Value -> Value -> Value
createValueKey key newValue userValue =
  case findValueKey key userValue of
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
specialKeys :: [Text]
specialKeys = ["Array", "Object"]

-- Go through each mapping in the configuration and map user keys to the api ones.
assignUserKeys :: FakieItem -> Value -> MappingContext
assignUserKeys FakieItem {..} apiValue =
  let mapping =
        -- initialize the context with empty error string and one empty Object
        flip execState (MappingContext "" (Object empty)) $
          mapM
            (\fm@FakieMap {..} ->
              -- we are using the special keys to access data
              if | fakieMapTheirkey `elem` specialKeys ->
                     case (fakieMapTheirkey, apiValue) of
                       -- if the string Array matches the actual json Array type
                       ("Array", Array arr) -> do
                         context  <- get
                         let newValue =
                               createValueKey fakieMapOurkey (Array arr) (mappingContextValue context)
                         modify'
                           (\mc ->
                              MappingContext
                                (mappingContextPossibleErrors mc)
                                newValue
                           )
                       -- if the string Object matches the actual json Object type
                       ("Object", Object o) -> do
                         context  <- get
                         let newValue =
                               createValueKey fakieMapOurkey (Object o) (mappingContextValue context)
                         modify'
                           (\mc ->
                              MappingContext (mappingContextPossibleErrors mc) newValue
                           )
                       -- for all the other cases we will present the user with the error.
                       -- If we decide to support some more special keys this is where they will go.
                       (specKey, _) ->
                         modify'
                           (\mc ->
                              MappingContext
                                (mappingContextPossibleErrors mc <> "Trying to use " <> specKey <> " as special key")
                                (mappingContextValue mc)
                           )
                 | otherwise ->
                    -- we are not using any special keys here.
                    -- we allow users to use dot notation like in js objects to look for a key inside of object
                    if | "." `T.isInfixOf` fakieMapTheirkey -> mapDotKeys fm
                       | otherwise ->
                           -- Try to find the api key inside of api results
                           case findValueKey fakieMapTheirkey apiValue of
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
      -- filter our the keys we should remove from final json
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
  where
    -- key was not found, alert the user about it
    noteKeyError :: Text -> State MappingContext ()
    noteKeyError notFoundKey =
      modify'
        (\mc ->
           MappingContext
             (mappingContextPossibleErrors mc <> "Key " <> notFoundKey <> " not found!")
             (mappingContextValue mc)
        )

    noteError :: Text -> State MappingContext ()
    noteError generalError =
      modify'
        (\mc ->
           MappingContext
             (mappingContextPossibleErrors mc <> generalError <> " not found!")
             (mappingContextValue mc)
        )
    -- map the keys containing "." (dot) accessors
    mapDotKeys :: FakieMap -> State MappingContext ()
    mapDotKeys FakieMap {..} = do
      let path = T.splitOn "." fakieMapTheirkey
      case take 2 path of
        [] -> noteKeyError fakieMapTheirkey
        _ -> do
         context <- get
         let mAlreadyMappedArrayOrObjectWithSpecialKey = ER.headMay path
             mPathToLookFor = ER.headMay (tail path)
         case (mAlreadyMappedArrayOrObjectWithSpecialKey, mPathToLookFor) of
           (Just alreadyMappedArrayOrObjectWithSpecialKey, Just pathToLookFor) ->
             let ourMappedValue = mappingContextValue context
             in
               case findValueKey alreadyMappedArrayOrObjectWithSpecialKey ourMappedValue of
                 Nothing -> noteKeyError alreadyMappedArrayOrObjectWithSpecialKey
                  -- we found the value, now drill into it with path contained after the dot
                 Just foundValue ->
                   case findValueKey pathToLookFor foundValue of
                     Nothing -> noteKeyError pathToLookFor
                     Just val ->
                       let newValue =
                              createValueKey fakieMapOurkey val ourMappedValue
                       in
                         modify'
                           (\mc ->
                              MappingContext (mappingContextPossibleErrors mc) newValue
                           )
           (_,_) -> noteKeyError (mconcat path)
