module EnvTests (envTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Env
import Eval
import History
import StdLib
import TestUtil
import Util
import Val

combineTest =
  let a = extend newEnv "x" (kI 10)
      b = extend newEnv "x" (kI 20)
      ab = a <> b
      ba = b <> a
      root = kI 10
      history = mkHistory root
   in testGroup ""
        [ elookup history ab "x" ~?= Just (kI 10)
        , elookup history ba "x" ~?= Just (kI 20)
        ]

envTests :: TestTree
envTests =
    testGroup "Test Suite"
    [ combineTest
    ]
