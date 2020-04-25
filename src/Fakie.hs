{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Fakie where

import qualified Control.Error          as ER
import           Control.Exception.Safe (MonadThrow, SomeException (..),
                                         throwIO, throwM, tryAny)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BSL
import           Data.Proxy
import           Debug.Pretty.Simple    (pTraceShowM)
import qualified Network.HTTP.Req       as R
import           System.Directory       (getCurrentDirectory, listDirectory)
import           System.FilePath        ((</>))
import qualified Text.URI               as URI
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
      mapM_
        (\apiItem -> do
           val <- liftIO $ callApi apiItem
           pTraceShowM val
        ) fakieConfig
      return fakieConfig

runGET
  :: R.Url scheme
  -> R.NoReqBody
  -> Proxy (R.JsonResponse Value)
  -> R.Option scheme
  -> R.Req (R.JsonResponse Value)
runGET = R.req R.GET

callApi :: (MonadIO m, MonadThrow m) => FakieItem -> m Value
callApi fItem@FakieItem {..} = do
  uri <- URI.mkURI fakieItemUrl
  urlAndOptions <-
    case R.useURI uri of
      Nothing -> throwIO (FakieException "Could not parse uri")
      Just urlAndOptions' -> return urlAndOptions'
  jsonResponse <-
    case urlAndOptions of
      Left (url, options')  ->
        let queryParams = constructQueryParams fItem
            options = options' <> queryParams
        in runReqCall fItem url options
      Right (url, options') ->
        let queryParams = constructQueryParams fItem
            options = options' <> queryParams
        in runReqCall fItem url options
  let rBody = R.responseBody jsonResponse
  return rBody

runReqCall
  :: (MonadIO m, FromJSON a)
  => FakieItem
  -> R.Url scheme
  -> R.Option scheme
  -> m (R.JsonResponse a)
runReqCall FakieItem {..} url options = R.runReq R.defaultHttpConfig $
  case fakieItemMethod of
    GET ->
      R.req R.GET url R.NoReqBody R.jsonResponse options
    _ -> fail "Not yet implemented"

constructQueryParams :: (R.QueryParam a, Monoid a) => FakieItem -> a
constructQueryParams FakieItem {..} =
  case fakieItemQueryParams of
    [] -> mempty
    qparams ->
      foldMap
      (\FakieQueryParam {..} ->
        fakieQueryParamName R.=: fakieQueryParamValue
      ) qparams

