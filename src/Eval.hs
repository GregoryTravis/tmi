module Eval
( eval ) where

import Env
import Lambda
import Util

import qualified Data.Map.Strict as M

eval :: Interp -> Lam -> Lam
eval interp@(Interp initialEnv _) lam =
  ep initialEnv lam
  where
    ep = e
    -- ep env x =
    --   let r = e env x
    --    in eesp ("eval", x, r) r
    e env l@(Lam arg body) = Closure env l
    e env (VId id) =
      case elookup env id of
        Just x -> ep env x
        Nothing ->
          error $ "Unknown identifier " ++ id
    e env (App (Closure cenv (Lam arg body)) x) =
      let ex = ep env x
          eenv = extend cenv arg ex
       in ep eenv body
    e env b@(Builtin name arity) =
      e env (BuiltinApp b [])
    e env (App (BuiltinApp b args) x) =
      -- TODO slow, remove append
      e env (BuiltinApp b (args ++ [e env x]))
    --e env (App a@(App _ _) y) =
      --e env (App (e env a) (e env y))
    e env a@(App f x)
      | isSelfEval f = a
      | otherwise = ep env app'
          where app' = App (ep env f) (ep env x)
    e _ ba@(BuiltinApp (Builtin name arity) args) =
      if length args == arity
         then evalBuiltin interp name args
         else ba

    -- Eval to self
    e _ x
      | isSelfEval x = x
        -- TODO remove?
      | otherwise = error $ "eval? " ++ show x

isSelfEval :: Lam -> Bool
isSelfEval x@(VI _) = True
isSelfEval x@(VS _) = True
isSelfEval x@(Closure _ _) = True
isSelfEval _ = False

evalBuiltin (Interp _ (BuiltinDefs bs)) name args =
  case M.lookup name bs of
    Nothing -> error $ "Unknown builtin " ++ name
    Just (BuiltinDef _ _ f) -> f args
