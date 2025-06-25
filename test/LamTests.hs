module LamTests (lamTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Code
import Display
import Lst
import Rec
import Rel
import Tuple
import Util
import Value

infix 1 ~?=
a ~?= e = testCase "" $ a @?= e

factTest =
  let fact = Lam "x" (If (App (App (Prim "==") (Var "x")) (Const (I 0)))
                        (Const (I 1))
                        (App (App (Prim "*") (Var "x"))
                             (App (Var "fact")
                                  (App (App (Prim "-") (Var "x")) (Const (I 1))))))
      factEvaled = eval M.empty M.empty fact
      genv = M.fromList [("fact", factEvaled)]
      result10 = eval genv M.empty (App (Var "fact") (Const (I 10)))
      result20 = eval genv M.empty (App (Var "fact") (Const (I 20)))
   in testGroup "fact" [
        result10 ~?= I 3628800
      , result20 ~?= I 2432902008176640000
      ]

lamTests :: TestTree
lamTests = testGroup "Test Suite" [
    factTest
  ]

