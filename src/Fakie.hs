{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Fakie where

import           Control.Concurrent.Async (mapConcurrently)
import qualified Control.Error            as ER
import           Control.Exception.Safe   (MonadThrow, SomeException (..),
                                           throwM, tryAny)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.CaseInsensitive     as CI
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Network.HTTP.Client      (parseUrlThrow)
import           Network.HTTP.Simple
import           System.Directory         (getCurrentDirectory, listDirectory)
import           System.FilePath          ((</>))
import           Types

readFakieConfig :: (MonadIO m, MonadThrow m) => m Fakie
readFakieConfig = do
  eFakie <- ER.runExceptT $ do
    cwd <- ER.ExceptT $ liftIO (tryAny getCurrentDirectory)
    fileList <- ER.ExceptT $ liftIO (tryAny $ listDirectory cwd)
    if configFileName `notElem` fileList
      then throwM (FakieException "No config file detected")
      else do
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile $ cwd </> configFileName)
        ER.hoistEither $ ER.fmapL (SomeException . FakieException) (eitherDecode fileContents :: Either String [FakieItem])
  case eFakie of
    Left err -> throwM (FakieException $ show err)
    Right fakieConfig -> do
      v <- liftIO $ mapConcurrently callApi fakieConfig
      liftIO $ print v
      return fakieConfig

callApi :: (MonadIO m, MonadThrow m) => FakieItem -> m Value
callApi fItem@FakieItem {..} = liftIO $ do
  erequest <-
    tryAny $
      parseUrlThrow (show fakieItemMethod <> " " <> T.unpack fakieItemUrl)
  case erequest of
    Left err -> throwM (FakieException (show err))
    Right request'' -> do
      let request' =
            setRequestQueryString (constructQueryParams fItem) request''
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
    Just body -> setRequestBodyLBS (encode body) r

setUpRequestHeaders :: Request -> FakieItem -> Request
setUpRequestHeaders r FakieItem {..} =
  let formattedHeaders =
        (\FakieHeader {..} ->
          (CI.mk $ encodeUtf8 fakieHeaderName, encodeUtf8 fakieHeaderValue)
        ) <$> fakieItemHeaders
  in setRequestHeaders formattedHeaders r

constructQueryParams :: FakieItem -> Query
constructQueryParams FakieItem {..} =
  (\FakieQueryParam {..} ->
    (encodeUtf8 fakieQueryParamName,  Just (encodeUtf8 fakieQueryParamValue))
  ) <$> fakieItemQueryParams
