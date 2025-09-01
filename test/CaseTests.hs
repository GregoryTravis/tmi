module CaseTests (caseTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Env
import Eval
import StdLib
import TestUtil
import Util
import Val

caseTest = 
  let lyst = App (App (Id "Cons") (CVal (kI 10)))
                 (App (App (Id "Cons") (CVal (kI 20)))
                      (Id "Nil"))
      lhd = App (Id "head") lyst
      ltl = App (Id "tail") lyst
   in testGroup ""
        [ (eval stdLib $ App (Id "head") lyst) ~?= (CVal (kI 10))
        , (eval stdLib $ App (Id "tail") lyst) ~?= (CVal (Val DK (Cton "Cons" [kI 20, Val DK (Cton "Nil" [])])))
        ]

caseTests :: TestTree
caseTests =
    testGroup "Test Suite"
    [ caseTest
    ]
