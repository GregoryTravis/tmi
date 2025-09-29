module Eval
( eval ) where

import Case
import Env
import History
import Pretty
import Util
import Val

import Data.List (intercalate)
import qualified Data.Map.Strict as M

verbose = True

eval :: Interp -> Code -> Code
eval interp@(Interp history _) code =
  ep 0 GlobalLayer code
  where
    ep :: Int -> Env -> Code -> Code
    ep level = if verbose then ev level else e level
    ev :: Int -> Env -> Code -> Code
    ev level env x =
      let pluses = intercalate "" (take level (repeat "+"))
          minuses = intercalate "" (take level (repeat "-"))
          r = e level env (eesp (pluses ++ " eval", pp x) x)
       in eesp (minuses ++ " eval", pp x, r) r
    e :: Int -> Env -> Code -> Code
    e l env lm@(Lam arg body) = CVal $ dkv $ Closure env lm
    e l env (Id id) =
      case elookup history env id of
        Just x -> ep (l+1) env $ CVal x
        Nothing ->
          error $ "Unknown identifier " ++ id
    e l env (CVal (Val _ (Code c))) =
      ep (l+1) env c
    e l env (App (CVal (Val _ (Closure cenv (Lam arg body)))) x) =
      let (CVal ex) = ep (l+1) env x
          eenv = extend cenv arg ex
       in ep (l+1) eenv body
    e l env a@(App f x) =
      ep (l+1) env app'
          where app' = App (ep (l+1) env f) (ep (l+1) env x)
    e l env (Builtin name args) =
        CVal $ evalBuiltin interp name (map (unCVal . (ep (l+1) env)) args)
    e l env (Ctor name args) =
        CVal $ Val DK $ Cton name (map (unCVal . (ep (l+1) env)) args)
    e l env (CtorRec name pairs) =
        let ids = map fst pairs
            exps = map snd pairs
            vals = map (unCVal . (ep (l+1) env)) exps
         in CVal $ Val DK $ CtonRec name (zip ids vals)
    e l env (If be th el) =
        let b = ep (l+1) env be
         in case b of
              CVal (Val _ (VB True)) -> ep (l+1) env th
              CVal (Val _ (VB False)) -> ep (l+1) env el
              _ -> error $ "If: not a bool" ++ show b
    e l env (Case xc cases) =
      let CVal x = ep (l+1) env xc
       in case match x cases of
            Nothing -> error $ "Pattern match failure: " ++ show x ++ " " ++ show cases
            Just (matchEnv, body) ->
              let env' = env <> matchEnv
               in ep (l+1) env' body
    -- Eval to self
    e _ _ x@(CVal _) = x
    -- e _ x = error $ "eval? " ++ show x

evalBuiltin :: Interp -> Ident -> [Val] -> Val
evalBuiltin (Interp _ (Outside { builtinDefs = BuiltinDefs bs })) name args =
  case M.lookup name bs of
    Nothing -> error $ "Unknown builtin " ++ name
    Just (BuiltinDef _ _ f) -> f args
