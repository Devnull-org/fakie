module Common
  ( Either (..)
  , Maybe (..)
  , Show (..)
  , Bool (..)
  , Int
  , IO
  , String
  , Text
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
  , Eq
  , FilePath
  , drop
  , flip
  , evalState
  , evalStateT
  , State
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
  , Word
  ) where


import           Control.Monad       (when)
import           Control.Monad.State (State, evalState, evalStateT, execState,
                                      execStateT, get, modify', put)
import           Control.Monad.Trans (lift)
import           Data.Bool           (bool)
import           Data.List           (filter, length, null)
import           Data.Maybe
import           Data.Text           (Text)
import           Debug.Pretty.Simple (pTraceShow, pTraceShowM)
import           Prelude             (Bool (..), Either (..), Eq, FilePath, IO, Word,
                                      Int, Maybe (..), Show (..), String,
                                      concat, const, drop, elem, error, flip,
                                      fmap, fromIntegral, fst, last, mapM, any,
                                      mapM_, notElem, otherwise, print, not, id,
                                      putStrLn, return, show, snd, undefined,
                                      ($), (+), (.), (/=), (<$>), (<>), (=<<),
                                      (==), (>>), (>), (<))
import           Text.Read           (readMaybe)
