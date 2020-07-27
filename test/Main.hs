{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}


module Main where

import           Common
import           Control.Exception.Safe (tryAny)
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.Reader   (runReaderT)
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import           Data.HashMap.Strict
import qualified Data.Vector            as V
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import           Mapping
import           Server                 (callApi, checkPath)
import           Text.RawString.QQ      (r)
import           Types

main :: IO Bool
main =
  checkParallel $
    Group "Test Mapping"
      [ ("prop_findValueKey", prop_findValueKey)
      , ("prop_callApi", prop_callApi)
      , ("prop_checkPath", prop_checkPath)
      ]

prop_checkPath :: Property
prop_checkPath = property $ do
  let
    route = "/api/someroute"
    pathInfo = ["api", "someroute"]
    checked = checkPath pathInfo route
  assert checked
  success

prop_callApi :: Property
prop_callApi = property $ do
  fakieItem <- forAll genFakieItem
  let fakieEnv =
        FakieEnv
          { fakieEnvConfigFile = ".fakie.json"
          , fakieEnvOutputToFile = Nothing
          , fakieEnvTesting = True
          , fakieEnvMapping = []
          }
  eapiCall <- tryAny . evalIO $ runReaderT (callApi fakieItem "") fakieEnv
  assert (isRight eapiCall)
  success

genFakieItem :: Gen FakieItem
genFakieItem = do
  name <- Gen.element ["stephanie", "lennart", "simon"]
  route <- Gen.element ["/", "/api", "/api/user","//api/whatever/"]
  method <- Gen.element [GET, POST, PUT, DELETE, HEAD, OPTIONS, PATCH]
  url <- Gen.element ["https://google.com", "https://facebook.com"]
  queryParam <- genFakieQueryParam
  header <- genFakieHeader
  return
    FakieItem
     { fakieItemName        = name
     , fakieItemRoute       = route
     , fakieItemMethod      = method
     , fakieItemUrl         = url
     , fakieItemQueryParams = [queryParam]
     , fakieItemHeaders     = [header]
     , fakieItemBody        = Nothing
     , fakieItemMapping     = []
     }

genFakieQueryParam :: Gen FakieQueryParam
genFakieQueryParam = do
  name <- Gen.element ["user", "post"]
  value <- Gen.element ["1", "2", "3", "4", "5"]
  return
    FakieQueryParam
      { fakieQueryParamName  = name
      , fakieQueryParamValue = value
      }

genFakieHeader :: Gen FakieHeader
genFakieHeader = do
  name <- Gen.element ["Content-Type"]
  value <- Gen.element ["application/json", "plain-text"]
  return
    FakieHeader
      { fakieHeaderName  = name
      , fakieHeaderValue = value
      }

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
    findInPath "a" objectWithEmptyList Nothing === Right emptyArray
    findInPath "a.Array.nth-0.Object.b" objectWithListOfObjects Nothing === Right (Number 1)
    findInPath "a.Array.nth-0.b" objectWithListOfObjects Nothing === Right (Number 1)
    findInPath "a.nth-0.b" objectWithListOfObjects Nothing === Right (Number 1)
    findInPath "a.Array.nth-1.Object.c" objectWithListOfObjects Nothing === Right (String "some String")
    findInPath "a.Array.nth-1.c" objectWithListOfObjects Nothing === Right (String "some String")
    findInPath "a.nth-1.c" objectWithListOfObjects Nothing === Right (String "some String")
    findInPath "a.Array.nth-2.Object.d" objectWithListOfObjects Nothing ===
      Right (Array $ V.fromList $ Number <$> [1,2,3,4])
    findInPath "a.Array.nth-2.d" objectWithListOfObjects Nothing ===
      Right (Array $ V.fromList $ Number <$> [1,2,3,4])
    findInPath "a.nth-2.d" objectWithListOfObjects Nothing ===
      Right (Array $ V.fromList $ Number <$> [1,2,3,4])
    findInPath "Array.nth-2.Object.id" listOfObjects Nothing === Right (Number 3)
    findInPath "Array.nth-1.id" listOfObjects Nothing === Right (Number 2)
    findInPath "nth-1.id" listOfObjects Nothing === Right (Number 2)

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
