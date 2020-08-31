module Common
  ( Either (..)
  , Maybe (..)
  , Show (..)
  , Bool (..)
  , Int
  , IO
  , String
  , Text
  , Eq
  , FilePath
  , State
  , Word
  , module Data.Maybe
  , undefined
  , concat
  , const
  , putStrLn
  , print
  , mapM_
  , mapM
  , error
  , fromIntegral
  , notElem
  , length
  , null
  , otherwise
  , return
  , drop
  , flip
  , evalState
  , evalStateT
  , execState
  , execStateT
  , modify'
  , get
  , put
  , lift
  , elem
  , fst
  , snd
  , bool
  , filter
  , readMaybe
  , pTraceShow
  , pTraceShowM
  , when
  , not
  , decodeUtf8
  , unpack
  , isRight
  , fromRight
  , (&&)
  , (=<<)
  , ($)
  , (+)
  , (.)
  , (/=)
  , (<$>)
  , (==)
  , id
  , any
  , fmap
  , last
  , (>>)
  , (<>)
  , (>)
  , (<)
  ) where


import           Control.Monad       (when)
import           Control.Monad.State (State, evalState, evalStateT, execState,
                                      execStateT, get, modify', put)
import           Control.Monad.Trans (lift)
import           Data.Bool           (bool)
import           Data.Either         (isRight, fromRight)
import           Data.List           (filter, length, null)
import           Data.Maybe
import           Data.Text           (Text, unpack)
import           Data.Text.Encoding  (decodeUtf8)
import           Debug.Pretty.Simple (pTraceShow, pTraceShowM)
import           Prelude             (Bool (..), Either (..), Eq, FilePath, IO,
                                      Int, Show (..), String, Word,
                                      any, concat, const, drop, elem, error,
                                      flip, fmap, fromIntegral, fst, id, last,
                                      mapM, mapM_, not, notElem, otherwise,
                                      print, putStrLn, return, show, snd,
                                      undefined, ($), (&&), (+), (.), (/=), (<),
                                      (<$>), (<>), (=<<), (==), (>), (>>))
import           Text.Read           (readMaybe)
