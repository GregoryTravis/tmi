module Main where

import qualified Data.Map.Strict as M

import Eval
import StdLib
import Util
import Val

main = do
  let fact10 = App (Id "fact") (CVal (kI 10))
  msp $ eval stdLib fact10
