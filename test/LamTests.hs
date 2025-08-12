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
  let main = app2 (VId "+") (App (VId "add1") (VI 10)) (App (VId "sub1") (VI 20))
   in eval stdLib main ~?= VI 30

factTest =
  let main = App (VId "fact") (VI 10)
   in eval stdLib main ~?= VI 3628800

lamTests :: TestTree
lamTests =
    testGroup "Test Suite"
    [ addTest
    , factTest
    ]
