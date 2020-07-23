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

jsonObjectWithListOfObjects :: ByteString
jsonObjectWithListOfObjects = [r|{"a":[{"b":1},{"c":"some String"},{"d":[1,2,3,4]}]} |]

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
    objectWithEmptyList <- decodeToValue jsonObjectWithEmptyList
    objectWithListOfObjects <- decodeToValue jsonObjectWithListOfObjects
    findInPath "a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "Object" objectWithEmptyList Nothing === Right (Object $ singleton "a" emptyArray)
    findInPath "Object.a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a.Array" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a.Array.nth-0.Object.b" objectWithListOfObjects Nothing === Right (Number 1)
    findInPath "a.Array.nth-1.Object.c" objectWithListOfObjects Nothing === Right (String "some String")
    findInPath "a.Array.nth-2.Object.d" objectWithListOfObjects Nothing ===
      Right (Array $ V.fromList $ Number <$> [1,2,3,4])
