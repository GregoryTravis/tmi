module FdTests (fdTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Fd
import Util
import Value

import TestUtil

fdTests :: TestTree
fdTests =
  let fds0 = mkFds [DR ["a", "b"] ["c", "d"]]
   in testGroup "fd" [
        isFdOf fds0 (DR ["a", "b"] ["c", "d"]) ~?= True
      , isFdOf fds0 (DR ["a", "b", "e"] ["c", "d"]) ~?= True
      , isFdOf fds0 (DR ["a", "b"] ["c"]) ~?= True
      , isFdOf fds0 (DR ["a", "b"] ["c", "d", "e"]) ~?= False
      , isFdOf fds0 (DR ["a"] ["c", "d"]) ~?= False
      ]
