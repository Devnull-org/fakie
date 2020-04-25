module Main where

import           Debug.Pretty.Simple (pTraceShowM)
import           Fakie

main :: IO ()
main = do
  putStrLn "Fakie Api server operational!"
  config <- readFakieConfig
  pTraceShowM config
