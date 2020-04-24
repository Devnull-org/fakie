{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module Fakie where

import qualified Control.Error          as ER
import           Control.Exception.Safe (Exception, MonadThrow,
                                         SomeException (..), throwIO, throwM, tryAny)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe             (fromMaybe)
import           Data.Proxy
import           Data.Text              (Text)
import qualified Network.HTTP.Req       as R
import           System.Directory       (getCurrentDirectory, listDirectory)
import           System.FilePath        ((</>))
import           Text.Casing            (camel)
import qualified Text.URI               as URI

configFileName :: FilePath
configFileName = ".fakie.json"

newtype FakieException = FakieException String deriving (Show, Eq)

instance Exception FakieException

data Method
  = GET
  | POST
  | PUT
  | DELETE
  | HEAD
  | OPTIONS
  | PATCH
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Method)

data FakieQueryParam =
  FakieQueryParam
    { fakieQueryParamName  :: Text
    , fakieQueryParamValue :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 15 }) ''FakieQueryParam)

data FakieHeader =
  FakieHeader
    { fakieHeaderName  :: Text
    , fakieHeaderValue :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 11 }) ''FakieHeader)

data FakieMap =
  FakieMap
    { fakieMapOurkey    :: Text
    , fakieMapTheirkey  :: Text
    , fakieMapFieldtype :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 8 }) ''FakieMap)

data FakieItem =
  FakieItem
   { fakieItemName        :: Text
   , fakieItemRoute       :: Text
   , fakieItemMethod      :: Method
   , fakieItemUrl         :: Text
   , fakieItemQueryParams :: [FakieQueryParam]
   , fakieItemHeaders     :: [FakieHeader]
   , fakieItemMapping     :: [FakieMap]
   } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 9 }) ''FakieItem)

newtype FakieApi =
  FakieApi
    { fakieApiItems :: [FakieItem]
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 8 }) ''FakieApi)

type Fakie = [FakieItem]

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
    Left (err :: SomeException) -> throwM (FakieException $ show err)
    Right fakieConfig -> do
      mapM_
        (\apiItem -> do
           val <- liftIO $ callApi apiItem
           liftIO $ print val
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
  let queryParams = constructQueryParams fItem
  uri <- URI.mkURI fakieItemUrl
  case R.useHttpsURI uri of
    Nothing -> throwIO (FakieException "Trying to use https on http route!")
    Just (url, _options) -> R.runReq R.defaultHttpConfig $ do
      v <- runGET url R.NoReqBody R.jsonResponse (fromMaybe mempty queryParams)
      let rBody = R.responseBody v
      return rBody

constructQueryParams :: (R.QueryParam a, Monoid a) => FakieItem -> Maybe a
constructQueryParams FakieItem {..} =
  case fakieItemQueryParams of
    [] -> Nothing
    qparams ->
      Just $
      foldMap
      (\FakieQueryParam {..} ->
        fakieQueryParamName R.=: fakieQueryParamValue
      ) qparams

-- constructHttpMethod :: R.HttpMethod method => Method -> method
-- constructHttpMethod method =
--   case method of
--     GET    -> R.GET
--     POST   -> R.POST
--     PUT    -> R.PUT
--     DELETE -> R.DELETE
