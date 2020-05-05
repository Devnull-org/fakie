{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception.Safe   (SomeException, tryAny, throwM)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.Trans
import           Fakie
import           System.Log.FastLogger
import           Types                    (FakieEnv (..), FakieException (..))

main :: IO ()
main = do
  let logFile = "/home/v0d1ch/code/fakie/.fakie.log"
  logger <- initializeFileLogging logFile
  liftIO $ do
    putStrLn "Fakie Api"
    putStrLn "reading configuration..."
  econfig <- readFakieConfig
  case econfig of
    Left (_ :: SomeException) -> liftIO $ do
      putStrLn "Configuration error!"
      putStrLn "Fakie config could not be obtained."
    Right fakieConfig -> liftIO $ do
      putStrLn "Configuration looks good"
      putStrLn "Calling configured endpoints to get the data..."

      let fakieEnv =
            FakieEnv
              { fakieEnvLogFile = Just logFile
              , fakieEnvLog = logFunction logger
              }
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
      putStrLn (show err)
      throwM (FakieException "Exiting... could not initialize the logger")
    Right (logger, _) -> return logger

logFunction :: TimedFastLogger -> LogStr -> IO ()
logFunction logger msg = logger (\time -> toLogStr time <> " : " <> msg <> "\n")
