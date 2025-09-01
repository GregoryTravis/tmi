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
      lhd = App (Id "head") lyst
      ltl = App (Id "tail") lyst
      ltltl = App (Id "tail") (App (Id "tail") lyst)
  mspp $ eval stdLib fact10
  mspp $ eval stdLib lyst
  mspp $ eval stdLib lhd
  mspp $ eval stdLib ltl
  mspp $ eval stdLib ltltl
