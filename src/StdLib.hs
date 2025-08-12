module StdLib (stdLib) where

import qualified Data.Map.Strict as M

import Builtin
import Env
import Interp
import Lambda
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

stdLib :: Interp
stdLib =
  let builtinDefMap = BuiltinDefs $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = Env $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, toBuiltinLam bd)
      globalEnv = combineNoClash nonBuiltins builtinEnv
   in mkInterp globalEnv builtinDefMap
