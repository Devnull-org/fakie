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
    { fakieEnvLogFile    :: FilePath
    , fakieEnvConfigFile :: FilePath
    , fakieEnvTesting    :: Bool
    }

newtype ServerOptions =
  ServerOptions
    { optPort :: Port
    } deriving Show

-- TODO: introduce store to file for the next version
-- storeToFile :: Maybe FilePath
data CmdOptions = CmdOptions
  { cmdOptionsConfigFile  :: Maybe FilePath
  , cmdOptionsLogFile     :: Maybe FilePath
  , cmdOptionsServerPort  :: Int
  } deriving (Eq, Show)
