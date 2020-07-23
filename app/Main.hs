{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           Control.Monad.Reader        (runReaderT)
import           Options.Applicative
import           Server
import           Types                       (CmdOptions (..))

options :: Parser CmdOptions
options =
  CmdOptions
    -- TODO: introduce this option in the next version
    -- <$> optional
    --      (strOption
    --       ( long "store-to-file"
    --        <> short 'f'
    --        <> help "If you don't want to run the server you can get all of the API json data in a file"
    --       )
    --     )
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
           ( long "log-file"
             <> short 'l'
             <> help "Provide your log file for Fakie server. \
                      \ Default place we log at is file named '.fakie.log'  \
                      \ in current directory."
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
  cmdOptions <- execParser opts
  runReaderT serverStart cmdOptions
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Fakie - merge multiple API endpoints into results you control."
     <> header "Fakie - The ultimate API glue!" )

