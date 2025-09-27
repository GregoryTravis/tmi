module Adt
( mkCtor
, ctonRecLookup
, ctonRecLookupMust ) where

import Builtin
import Util
import Val

mkCtor :: TyCtor -> Code
mkCtor (TyCtor name tys) =
  let arity = length tys
      vars = map (("x" ++) . show) [0..arity-1]
      wrap [] = Ctor name (map Id vars)
      wrap (v:vs) = Lam v (wrap vs)
   in wrap vars
mkCtor (TyCtorRec name pairs) =
  let arity = length pairs
      vars = map fst pairs
      wrap [] = CtorRec name (map (\x -> (x, Id x)) vars)
      wrap (v:vs) = Lam v (wrap vs)
   in wrap vars

ctonRecLookupMust :: Val -> Ident -> Val
ctonRecLookupMust x@(Val _ (CtonRec _ vals)) ident =
  case lookup ident vals of
    Just x -> x
    Nothing -> error $ "Lookup failure " ++ ident ++ " " ++ show x

ctonRecLookup :: Val -> Ident -> Maybe Val
ctonRecLookup x@(Val _ (CtonRec _ vals)) ident = lookup ident vals
