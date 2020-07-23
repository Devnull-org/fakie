{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Server where

import           Common
import qualified Control.Error            as ER
import           Control.Lens ((^.))
import           Control.Exception.Safe   (SomeException, throwM, tryAny, MonadThrow, SomeException (..))
import           Control.Monad.Reader     (runReaderT, ReaderT (..), MonadReader (..))
import           Data.Aeson (Value (..),decode, eitherDecode)
import qualified Data.Text                as T
import qualified Data.List as L
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Network.URI.Encode       (encodeTextToBS)
import           Network.Wai (Middleware, Request (..), Response, getRequestBodyChunk
                             , pathInfo)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Simple (Query, JSONException (..), setRequestBodyJSON
                                     , setRequestHeaders, getResponseStatusCode
                                     , getResponseBody, httpJSONEither, setRequestQueryString)
import qualified Data.CaseInsensitive     as CI
import           Network.Wai.Handler.Warp (defaultSettings, setPort, setServerName, runSettings)
import           Types                    (FakieEnv (..), FakieException (..), FakieItem (..)
                                          , FakieHeader (..), CmdOptions (..), Fakie
                                          , FakieQueryParam (..), MappingContext (..))
import           Network.Wai.Middleware.Cors (simpleCors)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import           Util                     (returnJson, returnJsonError)
import           Control.Concurrent.Async (mapConcurrently)
import           System.Directory         (getCurrentDirectory, listDirectory)
import           Control.Monad.Trans
import           System.FilePath          ((</>))
import           System.FilePath.Lens (filename)
import           Mapping (findValueKey, assignUserKeys)

serverStart :: (MonadIO m, MonadThrow m, MonadReader CmdOptions m) => m ()
serverStart = do
  cmdOptions@CmdOptions {..} <- ask
  let
    settings = setServerName "Fakie API Server" (setPort cmdOptionsServerPort defaultSettings)
    app = staticApp (defaultWebAppSettings "fakie")
  liftIO $ do
    putStrLn ("[INFO] Running server on port " <> show cmdOptionsServerPort)
    runSettings settings (logStdoutDev $ fakieMiddleware cmdOptions $ simpleCors app)

fakieMiddleware :: CmdOptions -> Middleware
fakieMiddleware options _ req f = do
  response <-
    case requestMethod req of
      "POST"  -> handlePostRequest options req
      _ ->
        throwM (FakieException $ "Method not handled " <>
                T.unpack (decodeUtf8 $ requestMethod req)
               )
  liftIO $ f response

redundantPaths :: [[Text]]
redundantPaths = [["favicon.ico"]]

handlePostRequest :: CmdOptions -> Request -> IO Response
handlePostRequest CmdOptions {..} req = do
  when (isNothing cmdOptionsLogFile) $
    putStrLn "[INFO] Could not find provided log file path. Defaulting to /.fakie.log"
  when (isNothing cmdOptionsConfigFile) $
    putStrLn "[INFO] Could not find provided configuration file path. Defaulting to /.fakie.json"
  let
    fakieLogFile = fromMaybe "/.fakie.log" cmdOptionsLogFile
    fakieConfigFile = fromMaybe "/.fakie.json" cmdOptionsConfigFile
    path = pathInfo req
  if path `elem` redundantPaths
    then return $ returnJson (String "Ignore requested favicon.ico")
    else liftIO $ do
      putStrLn "[INFO] Fakie Api"
      putStrLn "[INFO] reading configuration..."
      let fakieEnv =
            FakieEnv
              { fakieEnvLogFile = fakieLogFile
              , fakieEnvConfigFile = fakieConfigFile
              , fakieEnvTesting = False
              }
      econfig <- runReaderT readFakieConfig fakieEnv
      case econfig of
        Left (e :: SomeException) -> do
          putStrLn "[ERROR] Configuration error!"
          putStrLn "[ERROR] Fakie config could not be obtained."
          putStrLn "[ERROR] Please check the log file to see what went wrong"
          putStrLn $ "[ERROR] Configuration error : " <> show e
          throwM (FakieException "failure")
        Right fakieConfig -> do
          putStrLn $ "[INFO] Configuration read from " <> fakieConfigFile
          putStrLn "[INFO] Calling configured endpoints to get the data..."
          body <- getRequestBodyChunk req
          v <-
            mapConcurrently
             (\cfg -> flip runReaderT fakieEnv $ do
               eApiResponse <- tryAny (callApi cfg body)
               case eApiResponse of
                 Left err ->
                   return $
                     MappingContext
                       { mappingContextFailures = [T.pack $ show err]
                       , mappingContextErrors = []
                       , mappingContextValue = Null
                       }
                 Right apiResponse ->
                   return $ assignUserKeys cfg apiResponse
             ) fakieConfig
          putStrLn "[INFO] All calls are finished"
          let errors = mappingContextErrors <$> v
          if not (null errors)
            then
              return $
                returnJsonError
                  (String $ "Following mapped fields could not be found:" <>
                               T.intercalate "," (L.concat errors)
                  )
            else return $ returnJson (mappingContextValue <$> v)

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
    if (fakieEnvConfigFile ^. filename) `notElem` fileList
      then do
        liftIO .
          putStrLn $ "[ERROR] No config file detected! We tried to look into " <> fakieEnvConfigFile
        throwM (FakieException "No config file detected! Check the log file for errors.")
      else do
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile $ cwd </> fakieEnvConfigFile)
        ER.hoistEither $
          ER.fmapL (SomeException . FakieException) (eitherDecode fileContents :: Either String [FakieItem])

callApi
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader FakieEnv m
     )
  => FakieItem
  -> BS.ByteString
  -> m Value
callApi fItem rBody = do
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
             Client.parseUrlThrow
               (show (fakieItemMethod fItem) <> " "
                     <> T.unpack (fakieItemUrl fItem)
               )
         case erequest of
           Left err -> throwM (FakieException (show err))
           Right request'' -> do
             -- | All request bodies need to contain json key that matches the
             -- 'name' field in the json configuration for the specific configuration item
             -- If that key does not exist or we could not find it it means we are not trying to
             -- send any dynamic data to the endpoint.
             let
               decodedBody = decode (BSL.fromStrict rBody) :: Maybe Value
               eFoundJsonItemKey =
                 maybe (Left "" ) (findValueKey (fakieItemName fItem) Nothing) decodedBody
               bodyToSet =
                 case eFoundJsonItemKey of
                   Left _         -> fakieItemBody fItem
                   Right itemBody -> Just itemBody
               fakieItem = fItem { fakieItemBody = bodyToSet }
               request' =
                 setRequestQueryString (constructQueryParams fakieItem) $
                 setUpRequestHeaders request'' fakieItem
               request = setUpRequestBody request' fakieItem
             response <- httpJSONEither request
             case getResponseBody response :: Either JSONException Value of
               Left err -> throwM (FakieException (show err))
               Right respBody -> do
                 when (getResponseStatusCode response /= 200) $
                   throwM (FakieException "Received non 200 status code!")
                 return respBody

setUpRequestBody :: Client.Request -> FakieItem -> Client.Request
setUpRequestBody r FakieItem {..} =
  case fakieItemBody of
    Nothing   -> r
    Just body -> setRequestBodyJSON body r

setUpRequestHeaders :: Client.Request -> FakieItem -> Client.Request
setUpRequestHeaders r FakieItem {..} =
  let formattedHeaders =
        (\FakieHeader {..} ->
          (CI.mk $ encodeTextToBS fakieHeaderName, encodeTextToBS fakieHeaderValue)
        ) <$> fakieItemHeaders
  in setRequestHeaders formattedHeaders r

-- | Construct the 'Query' datatype from 'FakieQueryParams'. Basically a key value pairs.
constructQueryParams :: FakieItem -> Query
constructQueryParams FakieItem {..} =
  (\FakieQueryParam {..} ->
    (encodeTextToBS fakieQueryParamName, Just (encodeTextToBS fakieQueryParamValue))
  ) <$> fakieItemQueryParams
