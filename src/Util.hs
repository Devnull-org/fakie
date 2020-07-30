{-# LANGUAGE TemplateHaskell #-}

module Util
  ( InternalRoute (..)
  , internalRouteMethod
  , internalRouteResponse
  , internalRouteCheck
  , internalRouteHandler
  , returnHtml
  , returnText
  , returnJson
  , returnJsonError
  , returnJsonFatal
  , returnJsonNotAllowed
  , redirectSuccess
  , respond500
  ) where

import           Common
import           Control.Exception.Safe    (MonadThrow, throwM)
import           Control.Lens
import           Data.Aeson                (ToJSON, encode)
import qualified Data.ByteString.Lazy      as LBS
import           Network.HTTP.Types        (StdMethod (..), status200,
                                            status308, status402, status405,
                                            status409)
import           Network.HTTP.Types.Header (hContentType)
import           Network.URI               (parseURI)
import           Network.Wai
import           Network.Wai.Util          (redirect)
import           Types

data InternalRoute =
  InternalRoute
    { _internalRouteMethod   :: StdMethod
    , _internalRouteCheck    :: [String] -> Bool
    , _internalRouteHandler  :: [String] -> Response
    , _internalRouteResponse :: Response
    }

makeLenses ''InternalRoute

returnHtml :: LBS.ByteString -> Response
returnHtml =
  responseLBS
    status200
    [(hContentType, "text/html")]

returnText :: LBS.ByteString -> Response
returnText =
  responseLBS
    status200
    [(hContentType, "text/plain")]

returnJson :: ToJSON a => a -> Response
returnJson content =
  responseLBS
    status200
    [(hContentType, "application/json")]
    (encode content)

returnJsonError :: ToJSON a => a -> Response
returnJsonError content =
  responseLBS
    status402
    [(hContentType, "application/json")]
    (encode content)

returnJsonFatal :: ToJSON a => a -> Response
returnJsonFatal content =
  responseLBS
    status409
    [(hContentType, "application/json")]
    (encode content)

returnJsonNotAllowed :: ToJSON a => a -> Response
returnJsonNotAllowed content =
  responseLBS
    status405
    [(hContentType, "application/json")]
    (encode content)

redirectSuccess :: MonadThrow m => String -> m Response
redirectSuccess urlstring =
  case parseURI urlstring of
    Nothing  -> throwM (FakieException $ "Could not parse " <> urlstring <> " to proper uri!")
    Just uri ->
      case redirect status308 [] uri of
        Nothing       -> throwM (FakieException "Something went south with redirection")
        Just response -> return response

respond500 :: MonadThrow m => String -> m Response
respond500 errorString =
  throwM (FakieException errorString)
