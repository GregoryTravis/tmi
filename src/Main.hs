module Main where

import qualified Data.Map.Strict as M

import Awkward
import Eval
import Pretty
import StdLib
import Util
import Val

main = do
  let fact10 = App (Id "fact") (CVal (kI 10))
      lyst = mkListCode (map ckI [10, 20])
      lhd = App (Id "head") lyst
      ltl = App (Id "tail") lyst
      ltltl = App (Id "tail") (App (Id "tail") lyst)
      pats = [(Val DK (Cton "A" [Val DK (PatVar "a"), Val DK (PatVar "aa")]),
               App (App (Id "Cons") (Id "aa")) (App (App (Id "Cons") (Id "a")) (Id "Nil")))
             ,(Val DK (Cton "B" [kI 10, Val DK (PatVar "a")]),
               App (App (Id "Cons") (CVal (kI 100))) (App (App (Id "Cons") (Id "a")) (Id "Nil")))
             ]
      foo0 = Case (CVal (Val DK (Cton "A" [kI 10, kI 20]))) pats
      foo1 = Case (CVal (Val DK (Cton "B" [kI 10, kI 30]))) pats
  mspp $ eval stdLib fact10
  mspp $ eval stdLib lyst
  mspp $ eval stdLib lhd
  mspp $ eval stdLib ltl
  mspp $ eval stdLib ltltl
  mspp $ eval stdLib foo0
  mspp $ eval stdLib foo1
  mspp $ eval stdLib (App (App (Id "map") (Id "add1")) lyst)
