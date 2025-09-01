module StdLib (stdLib) where

import qualified Data.Map.Strict as M

import Builtin
import Env
import Interp
import Util
import Val

mkCtor0 :: String -> Val
mkCtor0 name = dkv $ Cton name []
mkCtor1 :: String -> Val -> Val
mkCtor1 name a = dkv $ Cton name [a]
mkCtor2 :: String -> Val -> Val -> Val
mkCtor2 name a b = dkv $ Cton name [a, b]

builtinDefs :: [BuiltinDef]
builtinDefs =
  [ (BuiltinDef "+" 2 (lyft2 (+) unVI unVI kI))
  , (BuiltinDef "-" 2 (lyft2 (-) unVI unVI kI))
  , (BuiltinDef "*" 2 (lyft2 (*) unVI unVI kI))
  , (BuiltinDef "==" 2 (lyft2 (==) id id kB))
  , (BuiltinDef "Cons" 2 (lyft2 (mkCtor2 "Cons") id id id))
  , (BuiltinDef "Nil" 0 (lyft0 (mkCtor0 "Nil") id))
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
  , ("head", Val DK $ Code $
      Lam "x" (Case (Id "x") [(Val DK (Cton "Cons" [Val DK (PatVar "x"), Val DK (PatVar "xs")]), Id "x"),
                              (Val DK (Cton "Nil" []), CVal (kS "error: head of empty list"))]))
  , ("tail", Val DK $ Code $
      Lam "x" (Case (Id "x") [(Val DK (Cton "Cons" [Val DK (PatVar "x"), Val DK (PatVar "xs")]), Id "xs"),
                              (Val DK (Cton "Nil" []), CVal (kS "error: tail of empty list"))]))
  ]

stdLib :: Interp
stdLib =
  let builtinDefMap = BuiltinDefs $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, bd)
      builtinEnv = Env $ M.fromList (map f builtinDefs)
        where f bd@(BuiltinDef name _ _) = (name, dkv $ Code $ wrapBuiltin bd)
      globalEnv = combineNoClash nonBuiltins builtinEnv
   in mkInterp globalEnv builtinDefMap
