{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}

module Main where

import           Colog                    (pattern W, cmap, fmtMessage,
                                               log,
                                           logTextStdout,
                                           usingLoggerT)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception.Safe   (SomeException)
import           Control.Monad.Trans
import qualified Data.Text                as T
import           Fakie
import           Common

main :: IO ()
main = usingLoggerT logAction $ do
  liftIO $ putStrLn "Fakie Api server started"
  econfig <- readFakieConfig
  case econfig of
    Left (err :: SomeException) -> do
      log W (T.pack . show $ err)
      liftIO $ putStrLn "Fakie config could not be obtained. More info is in the log files"
    Right fakieConfig -> do
      v <-
        liftIO $
          mapConcurrently
           (\cfg -> do
             apiResponse <- callApi cfg
             return $ assignUserKeys cfg apiResponse
           ) fakieConfig
      liftIO $ pTraceShowM v
      liftIO $ putStrLn "Fakie request done"
      return ()
  where
    logAction = cmap fmtMessage logTextStdout
