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
      lyst2 = mkListCode (map ckI [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
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
      halfLyst2 = app2 (Id "filter") (Lam "x" (app2 (Id "<") (Id "x") (ckI 5))) lyst2
      yeah0 = app2 (Id "Loo") (ckI 10) (ckI 20)
      yeah0foo = App (Id "foo") yeah0
      yeah0bar = App (Id "bar") yeah0
      consfun = app2 (Id "cons") (ckI 0) (Id "Nil")
      relist0 = app3 (Id "foldr") (Id "cons") (Id "Nil") lyst
      relist1 = app3 (Id "foldr") (Id "cons") (Id "Nil") lyst2
      relist2 = app3 (Id "foldl") (Id "snoc") (Id "Nil") lyst
      relist3 = app3 (Id "foldl") (Id "snoc") (Id "Nil") lyst2
      minus0 = app2 (Id "-") (ckI 10) (ckI 5)
      minus0f = app2 (App (Id "flip") (Id "-")) (ckI 10) (ckI 5)
      snoc0 = app2 (Id "snoc") (Id "Nil") (ckI 10)
  mspp $ eval stdLib fact10
  mspp $ eval stdLib lyst
  mspp $ eval stdLib lhd
  mspp $ eval stdLib ltl
  mspp $ eval stdLib ltltl
  mspp $ eval stdLib foo0
  mspp $ eval stdLib foo1
  mspp $ eval stdLib halfLyst2
  mspp $ eval stdLib (App (App (Id "map") (Id "add1")) halfLyst2)
  mspp $ eval stdLib yeah0foo
  mspp $ eval stdLib yeah0bar
  mspp $ eval stdLib consfun
  mspp $ eval stdLib (app2 (Id "cons") (ckI 10) (Id "Nil"))
  mspp $ eval stdLib relist0
  mspp $ eval stdLib relist1
  mspp $ eval stdLib relist2
  mspp $ eval stdLib relist3
  mspp $ eval stdLib minus0
  mspp $ eval stdLib minus0f
  mspp $ eval stdLib snoc0
  mspp $ eval stdLib lyst
  mspp $ eval stdLib (App (App (Id "map") (Id "add1")) lyst)
  mspp $ eval stdLib (App (App (Id "map2") (Id "add1")) lyst)
