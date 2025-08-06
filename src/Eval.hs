module Eval
( eval ) where

import Env
import Lambda
import Util

import qualified Data.Map.Strict as M

eval :: Interp -> Env -> Lam -> Lam
eval (Interp env _) l@(Lam arg body) = Closure env l
eval interp@(Interp env _) (App (Closure cenv (Lam arg body))) x =
  let ex = eval interp x
      eenv = extend cenv arg ex
   in eval int eenv body
eval interp@(Interp env _) (VId id) =
  case lookup interp id of
    Just x -> eval interp x
    Nothing ->
      error $ "Unknown identifier " ++ id
eval interp (App b@(Builtin name arity) x) =
  eval interp (BuiltinApp b [eval interp x])
eval interp ba@(BuiltinApp (Builtin name arity) args) =
  if length args == arity
     then evalBuiltin interp name args
     else ba

-- Eval to self
eval x@(VI _) = x
eval x@(VS _) = x
eval x@(Closure _ _) = x

evalBuiltin (Interp _ (BuiltinDefs bs)) name args =
  case M.lookup name bs of
    Nothing -> error $ "Unknown builtin " ++ name
    Just (BuiltinDef _ _ f) = f args
