module Main where

import Control.Monad.State hiding (lift)
import Data.Dynamic

import Log
import NestedState
import Reddit
import Test

main = logMain
