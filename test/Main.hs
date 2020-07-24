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

main :: IO Bool
main =
  checkParallel $ Group "Test Mapping" [
      ("prop_findValueKey", prop_findValueKey)
    ]

prop_findValueKey :: Property
prop_findValueKey =
  property $ do
    let emptyArray = Array V.empty
    objectWithEmptyList <- decodeToValue jsonObjectWithEmptyList
    objectWithListOfObjects <- decodeToValue jsonObjectWithListOfObjects
    listOfObjects <- decodeToValue jsonListOfObjects
    findInPath "a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "Object" objectWithEmptyList Nothing === Right (Object $ singleton "a" emptyArray)
    findInPath "Object.a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a.Array" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a.Array.nth-0.Object.b" objectWithListOfObjects Nothing === Right (Number 1)
    findInPath "a.Array.nth-1.Object.c" objectWithListOfObjects Nothing === Right (String "some String")
    findInPath "a.Array.nth-2.Object.d" objectWithListOfObjects Nothing ===
      Right (Array $ V.fromList $ Number <$> [1,2,3,4])
    findInPath "Array.nth-2.Object.id" listOfObjects Nothing === Right (Number 3)

decodeToValue :: MonadThrow m => ByteString -> m Value
decodeToValue bs =
  case eitherDecode bs :: Either String Value of
    Left err ->
      throwM (FakieException $ show err)
    Right body -> return body

jsonObjectWithEmptyList :: ByteString
jsonObjectWithEmptyList = [r|{"a":[]} |]

jsonObjectWithListOfObjects :: ByteString
jsonObjectWithListOfObjects = [r|{"a":[{"b":1},{"c":"some String"},{"d":[1,2,3,4]}]} |]

jsonListOfObjects :: ByteString
jsonListOfObjects =
  [r|
     [
       {
         "userId": 1,
         "id": 1,
         "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
         "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
       },
       {
         "userId": 1,
         "id": 2,
         "title": "qui est esse",
         "body": "est rerum tempore vitae\nsequi sint nihil reprehenderit dolor beatae ea dolores neque\nfugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\nqui aperiam non debitis possimus qui neque nisi nulla"
       },
       {
         "userId": 1,
         "id": 3,
         "title": "ea molestias quasi exercitationem repellat qui ipsa sit aut",
         "body": "et iusto sed quo iure\nvoluptatem occaecati omnis eligendi aut ad\nvoluptatem doloribus vel accusantium quis pariatur\nmolestiae porro eius odio et labore et velit aut"
       }
     ]
    |]
