module Main where

import qualified Data.Map.Strict as M

import Eval
import Pretty
import StdLib
import Util
import Val

main = do
  let fact10 = App (Id "fact") (CVal (kI 10))
      lyst = App (App (Id "Cons") (CVal (kI 10)))
                 (App (App (Id "Cons") (CVal (kI 20)))
                      (Id "Nil"))
  mspp $ eval stdLib fact10
  mspp $ eval stdLib lyst
