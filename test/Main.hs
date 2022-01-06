module Main where

import           Test.Tasty             (defaultMain, testGroup, localOption, TestTree)
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

suite :: TestTree
suite = testGroup "Test Suite" [
  testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- -- the following test does not hold
  -- , testCase "List comparison (same length)" $
  --     [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

main :: IO ()
main = defaultMain suite
