module Main where

import Data.Dynamic
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Code
import Display
import Fd
import Name
import Old
import OptLam
import Rec
import Rel
import Tuple
import Util
import Value

main = do
  --stuffMain
  oldMain
  --optLamMain
