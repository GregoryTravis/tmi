module Main where

import Data.Dynamic
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Builtin
import Env
import Eval
import Interp
import Lambda
import Util

builtinDefs :: [BuiltinDef]
builtinDefs =
  [ (BuiltinDef "+" 2 (lyft2 (+) unVI unVI VI))
  , (BuiltinDef "-" 2 (lyft2 (-) unVI unVI VI))
  ]

nonBuiltins = Env $ M.fromList $
  [ ("add1", Lam "x" (App (App (Builtin "+" 2) (VId "x")) (VI 1)))
  , ("sub1", Lam "x" (App (App (Builtin "-" 2) (VId "x")) (VI 1)))
  ]

main = do
  let builtinDefMap = BuiltinDefs $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = Env $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, toBuiltinLam bd)
      globalEnv = combineNoClash nonBuiltins builtinEnv
      interp = mkInterp globalEnv builtinDefMap
      main = App (App (VId "add1") (VI 10)) (VI 20)
      result = eval interp main
  msp result
