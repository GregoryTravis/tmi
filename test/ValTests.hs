module ValTests (valTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Awkward
import Eval
import StdLib
import TestUtil
import Util
import Val

addTest =
  let main = app2 (Id "+") (App (Id "add1") (CVal (kI 10))) (App (Id "sub1") (CVal (kI 20)))
   in eval stdLib main ~?= CVal (Val DK (VI 30))

factTest =
  let main = App (Id "fact") (CVal (kI 10))
   in eval stdLib main ~?= (CVal $ kI 3628800)

recTest =
  let yeah0 = app2 (Id "Loo") (ckI 10) (ckI 20)
      yeah0foo = App (Id "foo") yeah0
      yeah0bar = App (Id "bar") yeah0
   in testGroup ""
     [ eval stdLib yeah0foo ~?= ckI 10
     , eval stdLib yeah0bar ~?= ckI 20
     ]

valTests :: TestTree
valTests =
    testGroup "Test Suite"
    [ addTest
    , factTest
    , recTest
    ]
