{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Fakie where

import           Blaze.ByteString.Builder (Builder, fromByteString,
                                           fromLazyByteString, toLazyByteString)
import           Colog                    (pattern D, pattern I, LoggerT (..),
                                           Message, WithLog, cmap, fmtMessage,
                                           log, logTextStdout, usingLoggerT)
import qualified Control.Error            as ER
import           Control.Exception.Safe   (MonadThrow, SomeException (..),
                                           throwM, tryAny)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.CaseInsensitive     as CI
import           Data.Int                 (Int64)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Debug.Pretty.Simple      (pTraceShow)
import           Network.HTTP.Client      (GivesPopper, Request (..),
                                           RequestBody (..), parseUrlThrow)
import           Network.HTTP.Simple
import           Prelude                  (Either (..), Maybe (..), String,
                                           error, fromIntegral, notElem, print,
                                           return, show, ($), (.), (/=), (<$>),
                                           (<>))
import           System.Directory         (getCurrentDirectory, listDirectory)
import           System.FilePath          ((</>))
import           Types

deriving instance MonadThrow m => MonadThrow (LoggerT Message m)

readFakieConfig :: (WithLog env Message m, MonadIO m, MonadThrow m) => m (Either SomeException Fakie)
readFakieConfig =
  ER.runExceptT $ do
    cwd <- ER.ExceptT $ liftIO (tryAny getCurrentDirectory)
    fileList <- ER.ExceptT $ liftIO (tryAny $ listDirectory cwd)
    if configFileName `notElem` fileList
      then do
        throwM (FakieException "No config file detected")
      else do
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile $ cwd </> configFileName)
        ER.hoistEither $
          ER.fmapL (SomeException . FakieException) (eitherDecode fileContents :: Either String [FakieItem])

callApi :: (MonadIO m, MonadThrow m) => FakieItem -> m Value
callApi fItem@FakieItem {..} = liftIO $ do
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
      debugRequestContents request
      response <- httpJSONEither request
      case getResponseBody response :: Either JSONException Value of
        Left err -> throwM (FakieException (show err))
        Right rBody -> do
          when (getResponseStatusCode response /= 200) $
            throwM (FakieException "Received non 200 status code!")
          return rBody

debugRequestContents :: MonadIO m => Request -> m ()
debugRequestContents request = usingLoggerT logAction $ do
  log D "Request"
  log D "Host:"
  log D (decodeUtf8 . host $ request)
  log D "Port:"
  log D (T.pack . show . port $ request)
  log D "Method:"
  log D (decodeUtf8 . method $ request)
  log D "Path:"
  log D (decodeUtf8 . path $ request)
  log D "Query string:"
  log D (decodeUtf8 . queryString $ request)
  log D "Secure:"
  log D (T.pack . show . secure $ request)
  log D "Headers:"
  log D (T.pack . show $ requestHeaders request)
  let body' = requestBody request
  case simplify body' of
    Left (_, builder) -> do
      log D "Body:"
      log D (decodeUtf8 . BSL.toStrict $ toLazyByteString builder)
      return ()
    Right _ -> do
      log D "No support for streaming request bodies for now"
      return ()
  where
    logAction = cmap fmtMessage logTextStdout
    simplify :: RequestBody -> Either (Int64, Builder) (Maybe Int64, GivesPopper ())
    simplify (RequestBodyLBS lbs) = Left (BSL.length lbs, fromLazyByteString lbs)
    simplify (RequestBodyBS bs) = Left (fromIntegral $ BS.length bs, fromByteString bs)
    simplify (RequestBodyBuilder len b) = Left (len, b)
    simplify _ = error "Not interested in streaming bodies right now"

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

constructQueryParams :: FakieItem -> Query
constructQueryParams FakieItem {..} =
  (\FakieQueryParam {..} ->
    (encodeUtf8 fakieQueryParamName,  Just (encodeUtf8 fakieQueryParamValue))
  ) <$> fakieItemQueryParams
