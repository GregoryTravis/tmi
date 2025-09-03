module Eval
( eval ) where

import Case
import Env
import Util
import Val

import qualified Data.Map.Strict as M

verbose = False

eval :: Interp -> Code -> Code
eval interp@(Interp initialEnv _) code =
  ep initialEnv code
  where
    ev :: Env -> Code -> Code
    ev env x =
      let r = e env (eesp ("+eval", x) x)
       in eesp ("-eval", x, r) r
    ep :: Env -> Code -> Code
    ep = if verbose then ev else e
    e :: Env -> Code -> Code
    e env l@(Lam arg body) = CVal $ dkv $ Closure env l
    e env (Id id) =
      case elookup env id of
        Just x -> ep env $ CVal x
        Nothing ->
          error $ "Unknown identifier " ++ id
    e env (CVal (Val _ (Code c))) =
      e env c
    e env (App (CVal (Val _ (Closure cenv (Lam arg body)))) x) =
      let (CVal ex) = ep env x
          eenv = extend cenv arg ex
       in ep eenv body
    e env a@(App f x) =
      ep env app'
          where app' = App (ep env f) (ep env x)
    e env (Builtin name args) =
        CVal $ evalBuiltin interp name (map (unCVal . (ep env)) args)
    e env (Ctor name args) =
        CVal $ Val DK $ Cton name (map (unCVal . (ep env)) args)
    e env (If be th el) =
        let b = e env be
         in case b of
              CVal (Val _ (VB True)) -> e env th
              CVal (Val _ (VB False)) -> e env el
              _ -> error $ "If: not a bool" ++ show b
    e env (Case xc cases) =
      let CVal x = e env xc
       in case match x cases of
            Nothing -> error $ "Pattern match failure: " ++ show x ++ " " ++ show cases
            Just (matchEnv, body) ->
              let env' = env <> matchEnv
               in e env' body
    -- Eval to self
    e _ x@(CVal _) = x
    -- e _ x = error $ "eval? " ++ show x

evalBuiltin :: Interp -> Ident -> [Val] -> Val
evalBuiltin (Interp _ (BuiltinDefs bs)) name args =
  case M.lookup name bs of
    Nothing -> error $ "Unknown builtin " ++ name
    Just (BuiltinDef _ _ f) -> f args
