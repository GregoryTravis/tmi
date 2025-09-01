module EnvTests (envTests) where

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

combineTest = 
  let a = extend newEnv "x" (kI 10)
      b = extend newEnv "x" (kI 20)
      ab = a <> b
      ba = b <> a
   in testGroup ""
        [ elookup ab "x" ~?= Just (kI 20)
        , elookup ba "x" ~?= Just (kI 10)
        ]

envTests :: TestTree
envTests =
    testGroup "Test Suite"
    [ combineTest
    ]
