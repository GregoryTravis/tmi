module StdLibTests (stdLibTests) where

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

stdLibTest =
  let lyst = App (App (Id "Cons") (CVal (kI 10)))
                 (App (App (Id "Cons") (CVal (kI 20)))
                      (Id "Nil"))
   in testGroup ""
        [ (eval stdLib $ (App (App (Id "map") (Id "add1")) lyst)) ~?= CVal (Val DK (Cton "Cons" [kI 11, Val DK (Cton "Cons" [kI 21, Val DK (Cton "Nil" [])])]))
        ]

stdLibTests :: TestTree
stdLibTests =
    testGroup "Test Suite"
    [ stdLibTest
    ]
