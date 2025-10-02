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


blah x y = eval stdLib x ~?= y

factTest =
  let main = App (Id "fact") (CVal (kI 10))
   in eval stdLib main ~?= (CVal $ kI 3628800)

stdLibTest =
  let lyst = App (App (Id "Cons") (CVal (kI 10)))
                 (App (App (Id "Cons") (CVal (kI 20)))
                      (Id "Nil"))
      lyst2 = mkListCode (map ckI [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
      halfLyst2 = app2 (Id "filter") (Lam "x" (app2 (Id "<") (Id "x") (ckI 5))) lyst2
      yeah0 = app2 (Id "Loo") (ckI 10) (ckI 20)
      yeah0foo = App (Id "foo") yeah0
      yeah0bar = App (Id "bar") yeah0
   in testGroup ""
        [ blah (eval stdLib $ (App (App (Id "map") (Id "add1")) lyst)) (CVal (Val DK (Cton "Cons" [kI 11, Val DK (Cton "Cons" [kI 21, Val DK (Cton "Nil" [])])])))
        , blah halfLyst2 (CVal (mkList (map kI [0, 1, 2, 3, 4])))
        , blah (App (App (Id "map") (Id "add1")) halfLyst2) (CVal (mkList (map kI [1, 2, 3, 4, 5])))
        , blah (app2 (Id "+") (App (Id "add1") (CVal (kI 10))) (App (Id "sub1") (CVal (kI 20)))) (CVal (Val DK (VI 30)))
        , blah (App (Id "fact") (CVal (Val DK (VI 10)))) (CVal (Val DK (VI 3628800)))
        , blah (App (Id "fact") (CVal (Val DK (VI 10)))) (CVal (Val DK (VI 3628800)))
        , blah (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil"))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Cons" [Val DK (VI 20),Val DK (Cton "Nil" [])])])))
        , blah (App (Id "head") (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil")))))))))) (CVal (Val DK (VI 10)))
        , blah (App (Id "tail") (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil")))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 20),Val DK (Cton "Nil" [])])))
        , blah (App (Id "tail") (App (Id "tail") (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil"))))))))))) (CVal (Val DK (Cton "Nil" [])))
        , blah (Case (CVal (Val DK (Cton "A" [Val DK (VI 10),Val DK (VI 20)]))) [(Val DK (Cton "A" [Val DK (PatVar "a"),Val DK (PatVar "aa")]),App (App (Id "Cons") (Id "aa")) (App (App (Id "Cons") (Id "a")) (Id "Nil"))),(Val DK (Cton "B" [Val DK (VI 10),Val DK (PatVar "a")]),App (App (Id "Cons") (CVal (Val DK (VI 100)))) (App (App (Id "Cons") (Id "a")) (Id "Nil")))]) (CVal (Val DK (Cton "Cons" [Val DK (VI 20),Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Nil" [])])])))
        , blah (Case (CVal (Val DK (Cton "B" [Val DK (VI 10),Val DK (VI 30)]))) [(Val DK (Cton "A" [Val DK (PatVar "a"),Val DK (PatVar "aa")]),App (App (Id "Cons") (Id "aa")) (App (App (Id "Cons") (Id "a")) (Id "Nil"))),(Val DK (Cton "B" [Val DK (VI 10),Val DK (PatVar "a")]),App (App (Id "Cons") (CVal (Val DK (VI 100)))) (App (App (Id "Cons") (Id "a")) (Id "Nil")))]) (CVal (Val DK (Cton "Cons" [Val DK (VI 100),Val DK (Cton "Cons" [Val DK (VI 30),Val DK (Cton "Nil" [])])])))
        , blah (CVal (Val DK (Code (App (App (Id "filter") (Lam "x" (CVal (Val DK (Code (App (App (Id "<") (Id "x")) (CVal (Val DK (VI 5))))))))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 0)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 1)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 2)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 3)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 4)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 5)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 6)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 7)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 8)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 9)))) (Id "Nil"))))))))))))))))))))))))))))))))))))))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 0),Val DK (Cton "Cons" [Val DK (VI 1),Val DK (Cton "Cons" [Val DK (VI 2),Val DK (Cton "Cons" [Val DK (VI 3),Val DK (Cton "Cons" [Val DK (VI 4),Val DK (Cton "Nil" [])])])])])])))
        , blah (App (App (Id "map") (Id "add1")) (CVal (Val DK (Code (App (App (Id "filter") (Lam "x" (CVal (Val DK (Code (App (App (Id "<") (Id "x")) (CVal (Val DK (VI 5))))))))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 0)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 1)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 2)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 3)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 4)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 5)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 6)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 7)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 8)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 9)))) (Id "Nil")))))))))))))))))))))))))))))))))))))))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 1),Val DK (Cton "Cons" [Val DK (VI 2),Val DK (Cton "Cons" [Val DK (VI 3),Val DK (Cton "Cons" [Val DK (VI 4),Val DK (Cton "Cons" [Val DK (VI 5),Val DK (Cton "Nil" [])])])])])])))
        , blah (App (Id "foo") (CVal (Val DK (Code (App (App (Id "Loo") (CVal (Val DK (VI 10)))) (CVal (Val DK (VI 20)))))))) (CVal (Val DK (VI 10)))
        , blah (App (Id "bar") (CVal (Val DK (Code (App (App (Id "Loo") (CVal (Val DK (VI 10)))) (CVal (Val DK (VI 20)))))))) (CVal (Val DK (VI 20)))
        , blah (CVal (Val DK (Code (App (App (Id "cons") (CVal (Val DK (VI 0)))) (Id "Nil"))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 0),Val DK (Cton "Nil" [])])))
        , blah (CVal (Val DK (Code (App (App (Id "cons") (CVal (Val DK (VI 10)))) (Id "Nil"))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Nil" [])])))
        , blah (CVal (Val DK (Code (App (App (App (Id "foldr") (Id "cons")) (Id "Nil")) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil"))))))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Cons" [Val DK (VI 20),Val DK (Cton "Nil" [])])])))
        , blah (CVal (Val DK (Code (App (App (App (Id "foldr") (Id "cons")) (Id "Nil")) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 0)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 1)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 2)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 3)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 4)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 5)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 6)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 7)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 8)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 9)))) (Id "Nil"))))))))))))))))))))))))))))))))))))))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 0),Val DK (Cton "Cons" [Val DK (VI 1),Val DK (Cton "Cons" [Val DK (VI 2),Val DK (Cton "Cons" [Val DK (VI 3),Val DK (Cton "Cons" [Val DK (VI 4),Val DK (Cton "Cons" [Val DK (VI 5),Val DK (Cton "Cons" [Val DK (VI 6),Val DK (Cton "Cons" [Val DK (VI 7),Val DK (Cton "Cons" [Val DK (VI 8),Val DK (Cton "Cons" [Val DK (VI 9),Val DK (Cton "Nil" [])])])])])])])])])])])))
        , blah (CVal (Val DK (Code (App (App (App (Id "foldl") (Id "snoc")) (Id "Nil")) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil"))))))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 20),Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Nil" [])])])))
        , blah (CVal (Val DK (Code (App (App (App (Id "foldl") (Id "snoc")) (Id "Nil")) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 0)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 1)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 2)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 3)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 4)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 5)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 6)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 7)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 8)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 9)))) (Id "Nil"))))))))))))))))))))))))))))))))))))))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 9),Val DK (Cton "Cons" [Val DK (VI 8),Val DK (Cton "Cons" [Val DK (VI 7),Val DK (Cton "Cons" [Val DK (VI 6),Val DK (Cton "Cons" [Val DK (VI 5),Val DK (Cton "Cons" [Val DK (VI 4),Val DK (Cton "Cons" [Val DK (VI 3),Val DK (Cton "Cons" [Val DK (VI 2),Val DK (Cton "Cons" [Val DK (VI 1),Val DK (Cton "Cons" [Val DK (VI 0),Val DK (Cton "Nil" [])])])])])])])])])])])))
        , blah (CVal (Val DK (Code (App (App (Id "-") (CVal (Val DK (VI 10)))) (CVal (Val DK (VI 5))))))) (CVal (Val DK (VI 5)))
        , blah (CVal (Val DK (Code (App (App (App (Id "flip") (Id "-")) (CVal (Val DK (VI 10)))) (CVal (Val DK (VI 5))))))) (CVal (Val DK (VI (-5))))
        , blah (CVal (Val DK (Code (App (App (Id "snoc") (Id "Nil")) (CVal (Val DK (VI 10))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Nil" [])])))
        , blah (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil"))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 10),Val DK (Cton "Cons" [Val DK (VI 20),Val DK (Cton "Nil" [])])])))
        , blah (App (App (Id "map") (Id "add1")) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 10)))) (CVal (Val DK (Code (App (App (Id "Cons") (CVal (Val DK (VI 20)))) (Id "Nil")))))))))) (CVal (Val DK (Cton "Cons" [Val DK (VI 11),Val DK (Cton "Cons" [Val DK (VI 21),Val DK (Cton "Nil" [])])])))
        , blah yeah0foo (ckI 10)
        , blah yeah0bar (ckI 20)
        ]

stdLibTests :: TestTree
stdLibTests =
    testGroup "Test Suite"
    [ stdLibTest
    ]
