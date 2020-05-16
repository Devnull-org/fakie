{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception.Safe   (SomeException, throwM, tryAny)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.Trans
import           Mapping
import           System.Log.FastLogger
import           Types                    (FakieEnv (..), FakieException (..))

main :: IO ()
main = do
  let logFile = "/home/v0d1ch/code/fakie/.fakie.log"
  logger <- initializeFileLogging logFile
  liftIO $ do
    putStrLn "Fakie Api"
    putStrLn "reading configuration..."
  let fakieEnv =
        FakieEnv
          { fakieEnvLogFile = Just logFile
          , fakieEnvLog = logFunction logger
          , fakieEnvTesting = True
          }
  econfig <- runReaderT readFakieConfig fakieEnv
  case econfig of
    Left (e :: SomeException) -> do
      putStrLn "Configuration error!"
      putStrLn "Fakie config could not be obtained."
      putStrLn "Please check the log file to see what went wrong"
      let logIt = logFunction logger . toLogStr
      logIt ("Configuration error : " <> show e)
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
      return ()

initializeFileLogging :: FilePath -> IO TimedFastLogger
initializeFileLogging logFile = do
  let fileLogSpec = FileLogSpec logFile 1000 1
  timeFormat <- newTimeCache simpleTimeFormat
  let logType = LogFile fileLogSpec defaultBufSize
  eFastLogger <- tryAny $ newTimedFastLogger timeFormat logType
  case eFastLogger of
    Left err -> do
      print err
      throwM (FakieException "Exiting... could not initialize the logger")
    Right (logger, _) -> return logger

logFunction :: TimedFastLogger -> LogStr -> IO ()
logFunction logger msg = logger (\time -> toLogStr time <> " : " <> msg <> "\n")
