module Types
  ( Fakie
  , FakieItem (..)
  , FakieQueryParam (..)
  , FakieException (..)
  , FakieHeader (..)
  , FakieMap (..)
  , FakieEnv (..)
  , Method (..)
  , MappingContext (..)
  , ServerOptions (..)
  , CmdOptions (..)
  , FakieResult (..)
  ) where

import           Control.Exception.Safe (Exception)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text              (Text)
import           Text.Casing            (camel)
import           Common
import           Network.Wai.Handler.Warp (Port)


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
    { fakieMapOurkey     :: Text
    , fakieMapTheirkey   :: Text
    , fakieMapShouldKeep :: Maybe Bool
    } deriving (Eq, Show)

instance FromJSON FakieMap where
  parseJSON = withObject "FakieMap" $ \o -> do
    ourKey <- o .: "ourkey"
    theirKey <- o .: "theirkey"
    shouldKeep <- o .:? "shouldKeep"
    return $
      FakieMap
        { fakieMapOurkey     = ourKey
        , fakieMapTheirkey   = theirKey
        , fakieMapShouldKeep = shouldKeep
        }

$(deriveToJSON (defaultOptions { fieldLabelModifier = camel . drop 8 }) ''FakieMap)

data FakieItem =
  FakieItem
   { fakieItemName        :: Text
   , fakieItemRoute       :: Text
   , fakieItemMethod      :: Method
   , fakieItemUrl         :: Text
   , fakieItemQueryParams :: [FakieQueryParam]
   , fakieItemHeaders     :: [FakieHeader]
   , fakieItemBody        :: Maybe Value
   , fakieItemMapping     :: [FakieMap]
   } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 9 }) ''FakieItem)

newtype FakieApi =
  FakieApi
    { fakieApiItems :: [FakieItem]
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 8 }) ''FakieApi)

type Fakie = [FakieItem]

data MappingContext =
  MappingContext
   { mappingContextFailures :: [Text]
   , mappingContextErrors :: [Text]
   , mappingContextValue :: Value
   } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 14 }) ''MappingContext)

data FakieEnv =
  FakieEnv
    { fakieEnvConfigFile   :: FilePath
    , fakieEnvOutputToFile :: Maybe FilePath
    , fakieEnvTesting      :: Bool
    , fakieEnvMapping      :: Fakie
    }

newtype ServerOptions =
  ServerOptions
    { optPort :: Port
    } deriving Show

data CmdOptions = CmdOptions
  { cmdOptionsConfigFile   :: Maybe FilePath
  , cmdOptionsOutputToFile :: Maybe FilePath
  , cmdOptionsServerPort   :: Int
  } deriving (Eq, Show)

data FakieResult =
  FakieResult
    { fakieResultFailures :: Maybe Text
    , fakieResultValue    :: [Value]
    , fakieResultMessage  :: Text
    } deriving (Eq, Show)

$(deriveJSON (defaultOptions { fieldLabelModifier = camel . drop 11 }) ''FakieResult)
