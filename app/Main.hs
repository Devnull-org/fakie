{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Colog                    (pattern D, pattern I, Message,
                                           pattern W, WithLog, cmap, fmtMessage,
                                           log, logDebug, logInfo,
                                           logTextStdout, logWarning,
                                           usingLoggerT)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception.Safe   (SomeException)
import           Control.Monad.Trans
import qualified Data.Text                as T
import           Debug.Pretty.Simple      (pTraceShowM)
import           Fakie
import           Prelude                  (Either (..), IO, putStrLn, return,
                                           show, ($), (.))

main :: IO ()
main = usingLoggerT logAction $ do
  liftIO $ putStrLn "Fakie Api server started"
  econfig <- readFakieConfig
  case econfig of
    Left (err :: SomeException) -> do
      log W (T.pack . show $ err)
      liftIO $ putStrLn "Fakie config could not be obtained. More info is in the log files"
    Right fakieConfig -> do
      v <- liftIO $ mapConcurrently callApi fakieConfig
      return ()
  where
    logAction = cmap fmtMessage logTextStdout
