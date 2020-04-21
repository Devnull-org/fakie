{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

module Fakie where

import qualified Control.Error          as ER
import           Control.Exception.Safe (Exception, MonadThrow, throwM, tryAny)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import           System.Directory       (getCurrentDirectory, listDirectory)
import           System.FilePath        ((</>))

configFileName :: FilePath
configFileName = ".fakie.conf"

newtype FakieException = FakieException String deriving (Show, Eq)

instance Exception FakieException

data FakieCfg =
  FakieCfg
    { fakieCfgRaw :: BS.ByteString
    } deriving (Eq, Show)

readFakieConfig :: (MonadIO m, MonadThrow m) => m FakieCfg
readFakieConfig = do
  eFakieConfigBS <- ER.runExceptT $ do
    cwd <- ER.ExceptT $ liftIO (tryAny getCurrentDirectory)
    liftIO $ putStrLn cwd
    fileList <- ER.ExceptT $ liftIO (tryAny $ listDirectory cwd)
    if configFileName `notElem` fileList
      then throwM (FakieException "No config file detected")
      else do
        liftIO $ putStrLn (cwd </> configFileName)
        fileContents <- ER.ExceptT $ liftIO $ tryAny (BS.readFile $ cwd </> configFileName)
        return fileContents
  case eFakieConfigBS of
    Left err       -> throwM (FakieException $ show err)
    Right rawBytes -> return (FakieCfg rawBytes)
