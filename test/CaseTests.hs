module CaseTests (caseTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Awkward
import Env
import Eval
import StdLib
import TestUtil
import Util
import Val

caseTest =
  let lyst = mkListCode (map ckI [10, 20])
      lhd = App (Id "head") lyst
      ltl = App (Id "tail") lyst
      pats = [(Val DK (Cton "A" [Val DK (PatVar "a"), Val DK (PatVar "aa")]),
               App (App (Id "Cons") (Id "aa")) (App (App (Id "Cons") (Id "a")) (Id "Nil")))
             ,(Val DK (Cton "B" [kI 10, Val DK (PatVar "a")]),
               App (App (Id "Cons") (CVal (kI 100))) (App (App (Id "Cons") (Id "a")) (Id "Nil")))
             ]
      foo0 = Case (CVal (Val DK (Cton "A" [kI 10, kI 20]))) pats
      foo1 = Case (CVal (Val DK (Cton "B" [kI 10, kI 30]))) pats
   in testGroup ""
        [ (eval stdLib $ App (Id "head") lyst) ~?= (CVal (kI 10))
        , (eval stdLib $ App (Id "tail") lyst) ~?= (CVal (Val DK (Cton "Cons" [kI 20, Val DK (Cton "Nil" [])])))
        , (eval stdLib $ foo0) ~?= CVal (mkList (map kI [20, 10]))
        , (eval stdLib $ foo1) ~?= CVal (mkList (map kI [100, 30]))
        ]

caseTests :: TestTree
caseTests =
    testGroup "Test Suite"
    [ caseTest
    ]
