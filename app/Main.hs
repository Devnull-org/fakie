module Main where

import Fakie

main :: IO ()
main = do
  putStrLn "Fakie Api server operational!"
  config <- readFakieConfig
  print config
