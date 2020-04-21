{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module Fakie where

import qualified Control.Error          as ER
import           Control.Exception.Safe (Exception, MonadThrow, SomeException (..),
                                         throwM, tryAny)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as BSL
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Types     (StdMethod)
import           System.Directory       (getCurrentDirectory, listDirectory)
import           System.FilePath        ((</>))
import           Text.Casing            (camel)

configFileName :: FilePath
configFileName = ".fakie.conf"

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

data FakieApi =
  FakieApi
    { fakieApiItems :: [FakieItem]
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 8 }) ''FakieApi)

type Fakie = [FakieItem]

-- instance FromJSON Fakie where
--   parseJSON = withArray "[]" $ \arr ->
--     parseJSON arr

-- $(deriveJSON defaultOptions ''Fakie)

readFakieConfig :: (MonadIO m, MonadThrow m) => m Fakie
readFakieConfig = do
  eFakie <- ER.runExceptT $ do
    cwd <- ER.ExceptT $ liftIO (tryAny getCurrentDirectory)
    liftIO $ putStrLn cwd
    fileList <- ER.ExceptT $ liftIO (tryAny $ listDirectory cwd)
    if configFileName `notElem` fileList
      then undefined -- throwM (SomeException "No config file detected")
      else do
        liftIO $ putStrLn (cwd </> configFileName)
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile $ cwd </> configFileName)
        ER.hoistEither $ ER.fmapL (SomeException . FakieException) (eitherDecode fileContents :: Either String [FakieItem])
  case eFakie of
    Left (err :: SomeException) -> throwM (FakieException $ show err)
    Right fakieConfig -> do
      liftIO $ print fakieConfig
      return fakieConfig
