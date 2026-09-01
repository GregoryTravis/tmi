module Main where

import qualified Data.Map.Strict as M

import Awkward
import Eval
import Pretty
import StdLib
import Util
import Val

dump = False

doit x = do
  let r = eval stdLib x
  if dump then 
    smsp $ "        , blah (" ++ show x ++ ") (" ++ show r ++ ")"
  else
    mspp $ r

main = do
  let fact10 = App (Id "fact") (CVal (kI 10))
      lyst = mkListCode (map ckI [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  doit fact10
  doit lyst
  doit (App (App (Id "map") (Id "add1")) lyst)
  -- msp stdLib
