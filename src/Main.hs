module Main where

import Control.Monad.State hiding (lift)
import Data.Dynamic

import NestedState
import Test

main = testMain
_main = nestedStateMain
