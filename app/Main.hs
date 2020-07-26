{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           Control.Exception.Safe (throwM)
import           Control.Monad.Logger   (runStdoutLoggingT)
import           Control.Monad.Reader   (runReaderT)
import           Data.Time              (Day, fromGregorian, getCurrentTime,
                                         utctDay)
import           Options.Applicative
import           Server
import           Types                  (CmdOptions (..), FakieException (..))

trialExpiration :: Maybe Day
trialExpiration = Nothing

options :: Parser CmdOptions
options =
  CmdOptions
    <$> optional
        ( strOption
           ( long "configuration-file"
             <> short 'c'
             <> help "Provide your configuration for Fakie server. \
                      \ Default place we look at is file named '.fakie.json'  \
                      \ in current directory."
           )
         )
    <*> optional
        ( strOption
           ( long "output-file"
             <> short 'o'
             <> help "Provide a file path if you want to save the server response to file."
           )
         )
    <*>
      option auto
        ( long "port"
         <> short 'p'
         <> help "Specify the port for the Fakie server"
        )

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  when (isJust trialExpiration && Just today > trialExpiration) $
    throwM (FakieException "Trial license expired!")
  cmdOptions <- execParser opts
  runReaderT (runStdoutLoggingT serverStart) cmdOptions
  where
    opts :: ParserInfo CmdOptions
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Fakie - merge multiple API endpoints into results you control."
     <> header "Fakie - The ultimate API glue!" )
