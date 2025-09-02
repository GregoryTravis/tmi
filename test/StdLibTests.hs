module StdLibTests (stdLibTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Awkward
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
      lyst2 = mkListCode (map ckI [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
      halfLyst2 = app2 (Id "filter") (Lam "x" (app2 (Id "<") (Id "x") (ckI 5))) lyst2
   in testGroup ""
        [ (eval stdLib $ (App (App (Id "map") (Id "add1")) lyst)) ~?= CVal (Val DK (Cton "Cons" [kI 11, Val DK (Cton "Cons" [kI 21, Val DK (Cton "Nil" [])])]))
        , eval stdLib halfLyst2 ~?= CVal (mkList (map kI [0, 1, 2, 3, 4]))
        , eval stdLib (App (App (Id "map") (Id "add1")) halfLyst2) ~?= CVal (mkList (map kI [1, 2, 3, 4, 5]))
        ]

stdLibTests :: TestTree
stdLibTests =
    testGroup "Test Suite"
    [ stdLibTest
    ]
