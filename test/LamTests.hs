module LamTests (lamTests) where

import qualified Data.Map.Strict as M

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Builtin
import Env
import Eval
import Interp
import Lambda
import TestUtil
import Util

builtinDefs :: [BuiltinDef]
builtinDefs =
  [ (BuiltinDef "+" 2 (lyft2 (+) unVI unVI VI))
  , (BuiltinDef "-" 2 (lyft2 (-) unVI unVI VI))
  , (BuiltinDef "*" 2 (lyft2 (*) unVI unVI VI))
  , (BuiltinDef "==" 2 (lyft2 (==) id id VB))
  ]

nonBuiltins = Env $ M.fromList $
  [ ("add1", Lam "x" (app2 (VId "+") (VId "x") (VI 1)))
  , ("sub1", Lam "x" (app2 (VId "-") (VId "x") (VI 1)))
  , ("fact", Lam "x" (If (app2 (VId "==") (VId "x") (VI 0))
                         (VI 1)
                         (app2 (VId "*") (VId "x")
                               (App (VId "fact")
                                    (app2 (VId "-") (VId "x")
                                         (VI 1))))))
  ]

addTest interp = 
  let main = App (App (VId "+") (App (VId "add1") (VI 10))) (App (VId "sub1") (VI 20))
   in eval interp main ~?= VI 30

factTest interp =
  let main = App (VId "fact") (VI 10)
   in eval interp main ~?= VI 3628800

lamTests :: TestTree
lamTests =
  let builtinDefMap = BuiltinDefs $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = Env $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, toBuiltinLam bd)
      globalEnv = combineNoClash nonBuiltins builtinEnv
      interp = mkInterp globalEnv builtinDefMap
   in testGroup "Test Suite"
    [ addTest interp
    , factTest interp
    ]
