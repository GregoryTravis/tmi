module LamTests (lamTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Eval
import Lambda
import StdLib
import TestUtil
import Util

addTest = 
  let main = app2 (Id "+") (App (Id "add1") (CVal (kI 10))) (App (Id "sub1") (CVal (kI 20)))
   in eval stdLib main ~?= CVal (Val DK (VI 30))

factTest =
  let main = App (Id "fact") (CVal (kI 10))
   in eval stdLib main ~?= (CVal $ kI 3628800)

lamTests :: TestTree
lamTests =
    testGroup "Test Suite"
    [ addTest
    , factTest
    ]
