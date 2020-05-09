module Common
  ( Either (..)
  , Maybe (..)
  , Show (..)
  , Bool (..)
  , IO
  , String
  , module Data.Maybe
  , undefined
  , concat
  , putStrLn
  , print
  , mapM_
  , mapM
  , error
  , fromIntegral
  , notElem
  , otherwise
  , return
  , (=<<)
  , ($)
  , (+)
  , (.)
  , (/=)
  , (<$>)
  , (==)
  , fmap
  , last
  , (<>)
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
  , bool
  , filter
  , pTraceShow
  , pTraceShowM
  ) where


import           Control.Monad.State (State, evalState, evalStateT, execState,
                                      execStateT, get, modify', put)
import           Control.Monad.Trans (lift)
import           Data.Bool           (bool)
import           Data.List           (filter)
import           Data.Maybe
import           Debug.Pretty.Simple (pTraceShow, pTraceShowM)
import           Prelude             (Bool (..), Either (..), Eq, FilePath, IO,
                                      Maybe (..), Show (..), String, concat,
                                      drop, elem, error, flip, fmap,
                                      fromIntegral, last, mapM, mapM_, notElem,
                                      otherwise, print, putStrLn, return, show,
                                      undefined, ($), (+), (.), (/=), (<$>),
                                      (<>), (=<<), (==))
