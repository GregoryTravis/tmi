module DisplayTests (displayTests) where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

infix 1 ~?=
a ~?= e = testCase "" $ a @?= e

displayTests :: TestTree
displayTests = testGroup "Test Suite" [
    1 + 2 ~?= 3
  , 10 + 20 ~?= 30
  ]
