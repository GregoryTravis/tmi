module Eval
( eval ) where

import Env
import Lambda
import Util

import qualified Data.Map.Strict as M

eval :: Interp -> Lam -> Lam
eval interp@(Interp initialEnv _) lam =
  e initialEnv lam
  where
    e env l@(Lam arg body) = Closure env l
    e env (App (Closure cenv (Lam arg body)) x) =
      let ex = e env x
          eenv = extend cenv arg ex
       in e eenv body
    e env (VId id) =
      case elookup env id of
        Just x -> e env x
        Nothing ->
          error $ "Unknown identifier " ++ id
    e env (App b@(Builtin name arity) x) =
      (BuiltinApp b [e env x])
    e _ ba@(BuiltinApp (Builtin name arity) args) =
      if length args == arity
         then evalBuiltin interp name args
         else ba

    -- Eval to self
    e _ x@(VI _) = x
    e _ x@(VS _) = x
    e _ x@(Closure _ _) = x

    -- TODO remove
    e _ x = error $ "eval? " ++ show x

evalBuiltin (Interp _ (BuiltinDefs bs)) name args =
  case M.lookup name bs of
    Nothing -> error $ "Unknown builtin " ++ name
    Just (BuiltinDef _ _ f) -> f args
