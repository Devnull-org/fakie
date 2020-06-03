{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           Control.Monad.Reader     (runReaderT)
import           Network.Wai.Handler.Warp (runSettings)
import           Options.Applicative
import           Server
import           Types                    (CmdOptions (..), ServerOptions (..), Fakie)

-- serverOptions :: Parser ServerOptions
-- serverOptions =
--   ServerOptions
--     <$> option auto
--         ( long "p"
--          <> help "Set the server port"
--          <> metavar "INT" )

-- serve :: IO ()
-- serve = do
--   _ <- execParser opts
--   return ()
--   where
--     opts = info (serverOptions <**> helper)
--       ( fullDesc
--      <> progDesc "Fakie server - serves your json content"
--      <> header "Fakie: Your Json toolbox for the web" )

options :: Parser CmdOptions
options =
  CmdOptions
    <$> optional
         (strOption
          ( long "store-to-file"
           <> short 'f'
           <> help "If you don't want to run the server you can get all of the API json data in a file"
          )
        )

main :: IO ()
main = do
  cmdOptions <- execParser opts
  settings <- runReaderT (serverStart (ServerOptions 4500)) cmdOptions
  runSettings settings app
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Fakie - merge multiple API endpoints into results you control."
     <> header "Fakie - The ultimate API glue!" )

