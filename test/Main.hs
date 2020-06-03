{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}


module Main where

import           Control.Monad.Catch  (MonadThrow (..))
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict
import qualified Data.Vector          as V
import           Hedgehog
import           Mapping
import           Prelude
import           Text.RawString.QQ    (r)
import           Types

jsonObjectWithEmptyList :: ByteString
jsonObjectWithEmptyList = [r|{"a":[]} |]

main :: IO Bool
main =
  checkParallel $ Group "Test Mapping" [
      ("prop_findValueKey", prop_findValueKey)
    ]

decodeToValue :: MonadThrow m => ByteString -> m Value
decodeToValue bs =
  case eitherDecode bs :: Either String Value of
    Left err ->
      throwM (FakieException $ show err)
    Right body -> return body

prop_findValueKey :: Property
prop_findValueKey =
  property $ do
    let emptyArray = Array V.empty
    val <- decodeToValue jsonObjectWithEmptyList
    findInPath "a" val Nothing === Right emptyArray
    findInPath "a" val Nothing === Right emptyArray
    findInPath "Object" val Nothing === Right (Object $ singleton "a" emptyArray)
    findInPath "Object.a" val Nothing === Right emptyArray
    findInPath "a.Array" val Nothing === Right emptyArray
