module Main where

import Data.Dynamic
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Builtin
import Lambda
import Util

builtinDefs :: Builtins
builtinDefs = Builtins
  [ ("+", 2, lyft2 unVI unVI VI +)
  , ("-", 2, lyft2 unVI unVI VI -)
  ]

nonBuiltins = Env $ fromList $
  [ ("add1", Lam "x" (Builtin 2 "+"))
  , ("sub1", Lam "x" (Builtin 2 "-"))
  ]

main = do
  let builtinEnv = map toBuiltinLam builtinDefs
      globalEnv = combineNoClash nonBuiltins builtinDefs
      interp = mkInterp globalEnv builtinDefs
      main = App (App (VId "add1") (VI 10)) (VI 20)
   in eval interp main
