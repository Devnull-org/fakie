module Server where

import           Common
import Data.Aeson (Value (..))
import           Control.Exception.Safe   (SomeException, throwM)
import           Control.Monad.Reader     (runReaderT, ReaderT)
import           Data.ByteString.Lazy     hiding (elem, length, null, putStrLn)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           Network.Wai
import           Network.Wai.Handler.Warp (defaultSettings,
                                           setPort, setServerName, Settings)
import           Types                    (FakieEnv (..), FakieException (..), CmdOptions (..),
                                           ServerOptions (..), MappingContext (..))
import           Util                     (respond500, returnJson)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.Trans
import           Mapping

serverStart :: ServerOptions -> ReaderT CmdOptions IO Settings
serverStart (ServerOptions port) = do
  lift $ putStrLn ("Running server on port " <> show port)
  let settings = setServerName "Fakie API Server" (setPort port defaultSettings)
  return settings

app
  :: Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
app req f = do
  print $ requestMethod req
  response <-
    case requestMethod req of
      "GET"  -> handleGetRequest req
      _ -> throwM (FakieException $ "Method not handled " <> T.unpack (decodeUtf8 $ requestMethod req))
  f response

redundantPaths :: [[Text]]
redundantPaths = [["favicon.ico"]]

handleGetRequest :: Request -> IO Response
handleGetRequest req = do
  let logFile = "/home/v0d1ch/code/fakie/.fakie.log"
  let path = pathInfo req
  pTraceShowM (path `elem` redundantPaths)
  if path `elem` redundantPaths
    then return $ returnJson (String "Ignore requested favicon.ico")
    else liftIO $ do
      pTraceShowM path
      putStrLn "Fakie Api"
      putStrLn "reading configuration..."
      let fakieEnv =
            FakieEnv
              { fakieEnvLogFile = Just logFile
              , fakieEnvLog = Nothing
              , fakieEnvTesting = False
              }
      econfig <- runReaderT readFakieConfig fakieEnv
      case econfig of
        Left (e :: SomeException) -> do
          putStrLn "Configuration error!"
          putStrLn "Fakie config could not be obtained."
          putStrLn "Please check the log file to see what went wrong"
          putStrLn ("Configuration error : " <> show e)
          throwM (FakieException "failure")
        Right fakieConfig -> do
          putStrLn "Configuration looks good"
          putStrLn "Calling configured endpoints to get the data..."
          v <-
              mapConcurrently
               (\cfg -> flip runReaderT fakieEnv $ do
                 apiResponse <- callApi cfg
                 return $ assignUserKeys cfg apiResponse
               ) fakieConfig
          putStrLn "All calls are finished"
          pTraceShowM v
          putStrLn "Fakie done"
          let errors = T.concat $ mappingContextPossibleErrors <$> v
          if T.length errors > 1
            then respond500 (T.unpack errors)
            else return $ returnJson (mappingContextValue <$> v)

checkRequestParams :: [ByteString] -> Either Text [ByteString]
checkRequestParams rp
  | null rp        = Left "Seems like you submitted the empty form...What? I don't do frontend validation, handle your shit!"
  | length rp /= 3 = Left "Last time I checked we need url, method and a valid json"
  | "" `elem` rp   = Left "Some of the params match empty quotes"
  | otherwise      = Right rp

ignoreParams :: [[Text]]
ignoreParams =
  [ ["favicon.ico"]
  , []
  ]

-- internalRoutes :: M.Map ByteString InternalRoute
-- internalRoutes =
--   M.fromList
--     [ (""                , InternalRoute GET  (const True) (\_ -> returnJson "a") )
--     , ("/"               , InternalRoute GET  (const True) (\_ -> returnJson "a") )
--     , ("/localhost-buddy", InternalRoute POST (const True) (\_ -> returnJson "a") )
--     ]

