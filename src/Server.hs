{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import           Common
import           Control.Concurrent.Async             (mapConcurrently)
import qualified Control.Error                        as ER
import           Control.Exception.Safe               (MonadCatch, MonadThrow,
                                                       SomeException (..),
                                                       throwM, tryAny)
import           Control.Monad.Logger                 (MonadLogger, logErrorN,
                                                       logInfoN,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadReader (..),
                                                       ReaderT (..), runReaderT)
import           Control.Monad.Trans
import           Data.Aeson                           (Value (..), decode,
                                                       eitherDecode, encode,
                                                       toJSON)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.CaseInsensitive                 as CI
import qualified Data.List                            as L
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           Mapping                              (assignUserKeys,
                                                       findValueKey)
import qualified Network.HTTP.Client                  as Client
import           Network.HTTP.Simple                  (JSONException (..),
                                                       Query, getResponseBody,
                                                       getResponseStatusCode,
                                                       httpJSONEither,
                                                       setRequestBodyJSON,
                                                       setRequestHeaders,
                                                       setRequestQueryString)
import           Network.URI.Encode                   (encodeTextToBS)
import           Network.Wai                          (Middleware, Request (..),
                                                       Response,
                                                       getRequestBodyChunk,
                                                       pathInfo)
import           Network.Wai.Application.Static       (defaultWebAppSettings,
                                                       staticApp)
import           Network.Wai.Handler.Warp             (defaultSettings,
                                                       runSettings, setPort,
                                                       setServerName)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Directory                     (getCurrentDirectory)
import           System.FilePath                      ((</>))
import           Types                                (CmdOptions (..), Fakie,
                                                       Fakie, FakieEnv (..),
                                                       FakieException (..),
                                                       FakieHeader (..),
                                                       FakieItem (..),
                                                       FakieQueryParam (..),
                                                       FakieResult (..),
                                                       MappingContext (..))
import           Util                                 (returnJson,
                                                       returnJsonError)

serverStart
  :: ( MonadIO m
     , MonadThrow m
     , MonadLogger m
     , MonadReader CmdOptions m
     )
  => m ()
serverStart = do
  CmdOptions {..} <- ask
  when (isNothing cmdOptionsConfigFile) $
    logInfoN "Could not find provided configuration file path. Defaulting to /.fakie.json"
  cwd <- liftIO getCurrentDirectory
  let
    fakieConfigFile = fromMaybe (cwd </> "/.fakie.json") cmdOptionsConfigFile
    fakieEnv =
      FakieEnv
        { fakieEnvConfigFile = fakieConfigFile
        , fakieEnvOutputToFile = cmdOptionsOutputToFile
        , fakieEnvTesting = False
        , fakieEnvMapping = []
        }
  logInfoN "Fakie Api"
  logInfoN "reading configuration..."
  econfig <- runReaderT readAndDecodeFakieConfig fakieEnv
  case econfig of
    Left (e :: SomeException) -> do
      logErrorN "Configuration error!"
      logErrorN "Fakie config could not be obtained."
      logErrorN ("Configuration error : " <> T.pack (show e))
      throwM (FakieException "Configuration file failure. Please double check the file path.")
    Right fakieConfig -> do
      logInfoN $ "Configuration read from " <> T.pack fakieConfigFile
      logInfoN "Calling configured endpoints to get the data..."
      let
        fakieEnvWithMapping = fakieEnv { fakieEnvMapping = fakieConfig }
        settings = setServerName "Fakie API Server" (setPort cmdOptionsServerPort defaultSettings)
        app = staticApp (defaultWebAppSettings "fakie")
      logInfoN ("Running server on port " <> T.pack (show cmdOptionsServerPort))
      liftIO $ runSettings settings (logStdoutDev $ fakieMiddleware fakieEnvWithMapping $ simpleCors app)

fakieMiddleware :: FakieEnv -> Middleware
fakieMiddleware fakieEnv _ req f = do
  response <-
    case requestMethod req of
      "POST"  -> runStdoutLoggingT (flip runReaderT fakieEnv $ handlePostRequest req)
      _ ->
        throwM (FakieException $ "Method not handled " <>
                T.unpack (decodeUtf8 $ requestMethod req)
               )
  liftIO $ f response

redundantPaths :: [[Text]]
redundantPaths = [["favicon.ico"]]

-- | Check if the current path matches the one user specified in configuration
checkPath :: [Text] -> Text -> Bool
checkPath currentRoute rawRoute =
  let wantedRoute = filter ("" /= ) (T.splitOn "/" rawRoute)
  in currentRoute == wantedRoute

handlePostRequest
  :: ( MonadIO m
     , MonadCatch m
     , MonadLogger m
     , MonadReader FakieEnv m
     )
  => Request
  -> m Response
handlePostRequest req = do
  fakieEnv <- ask
  let path = pathInfo req
  if path `elem` redundantPaths
    then return $ returnJson (String "Ignore requested favicon.ico")
    else do
      body <- liftIO $ getRequestBodyChunk req
      v <- liftIO $
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
               -- if current path matches the one in configuration try to assign user provided fields
               -- otherwise return empty results
               if checkPath path (fakieItemRoute cfg)
                 then return $ assignUserKeys cfg apiResponse
                 else
                   return
                     MappingContext
                       { mappingContextFailures = []
                       , mappingContextErrors = []
                       , mappingContextValue = Null
                       }

         ) (fakieEnvMapping fakieEnv)
      logInfoN "All calls are finished"
      let
        errors = mappingContextErrors <$> v
        thereAreFatalErrors = False `elem` (null <$> errors)
      if thereAreFatalErrors
        then
          return $ returnJsonError
            ( String $
              "Following mapped fields could not be found:" <>
              T.intercalate "," (L.concat errors)
            )
        else do
          eWritten <- tryAny (maybeOutputToFile (fakieEnvOutputToFile fakieEnv) (toJSON $ mappingContextValue <$> v))
          case eWritten of
            Left err -> throwM (FakieException $ "Could not write to output file! " <> show err)
            Right _ -> do
              let
                failList = L.concat $ mappingContextFailures <$> v
                failures
                  | not (null failList) = Just $ T.intercalate "," failList
                  | otherwise = Nothing
                response =
                  FakieResult
                    { fakieResultFailures = failures
                    , fakieResultValue =
                        maybe
                          (mappingContextValue <$> v)
                          (const [])
                          (fakieEnvOutputToFile fakieEnv)
                    , fakieResultMessage =
                        maybe
                          ""
                          (\outputPath ->
                              "Response redirected to file: " <> T.pack outputPath
                          )
                          (fakieEnvOutputToFile fakieEnv)
                    }
              return $ returnJson (toJSON response)

maybeOutputToFile :: MonadIO m => Maybe FilePath -> Value -> m ()
maybeOutputToFile mOutputToFile val =
  when (isJust mOutputToFile) $
    liftIO $ BSL.writeFile (fromJust mOutputToFile) (encode val)

readAndDecodeFakieConfig
  :: ( MonadIO m
     , MonadThrow m
     , MonadReader FakieEnv m
     , MonadLogger m)
  => m (Either SomeException Fakie)
readAndDecodeFakieConfig =
  ER.runExceptT $ do
    FakieEnv {..} <- lift ask
    fileContents <- ER.ExceptT $ liftIO $ tryAny (BSL.readFile fakieEnvConfigFile)
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
