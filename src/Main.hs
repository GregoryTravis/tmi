module Main where

import qualified Data.Map.Strict as M

import Eval
import Lambda
import StdLib
import Util

main = do
  let fact10 = App (VId "fact") (VI 10)
  msp $ eval stdLib fact10
