module Main2 where

import           Test.Tasty             (defaultMain, testGroup, localOption, TestTree)
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

main2suite :: TestTree
main2suite = testGroup "Test Suite" [
  testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
