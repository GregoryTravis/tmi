module StdLib (stdLib) where

import qualified Data.Map.Strict as M

import Builtin
import Env
import Interp
import Util
import Val

builtinDefs :: [BuiltinDef]
builtinDefs =
  [ (BuiltinDef "+" 2 (lyft2 (+) unVI unVI kI))
  , (BuiltinDef "-" 2 (lyft2 (-) unVI unVI kI))
  , (BuiltinDef "*" 2 (lyft2 (*) unVI unVI kI))
  , (BuiltinDef "==" 2 (lyft2 (==) id id kB))
  ]

nonBuiltins = Env $ M.fromList $
  [ ("add1", Val (TFun TI (TFun TI TI))
                 $ Code $ Lam "x" (app2 (Id "+") (Id "x") (CVal (kI 1))))
  , ("sub1", Val (TFun TI (TFun TI TI))
                 $ Code $ Lam "x" (app2 (Id "-") (Id "x") (CVal (kI 1))))
  , ("fact", Val (TFun TI TI)
                 $ Code $ Lam "x" (If (app2 (Id "==") (Id "x") (CVal (kI 0)))
                                      (CVal (kI 1))
                                      (app2 (Id "*") (Id "x")
                                            (App (Id "fact")
                                                 (app2 (Id "-") (Id "x")
                                                       (CVal (kI 1)))))))
  ]

stdLib :: Interp
stdLib =
  let builtinDefMap = BuiltinDefs $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = Env $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, dkv $ Code $ toBuiltinLam bd)
      globalEnv = combineNoClash nonBuiltins builtinEnv
   in mkInterp globalEnv builtinDefMap
