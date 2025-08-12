module Env
( newEnv
, extend
, combineNoClash
, elookup ) where

import qualified Data.Map.Strict as M

import Lambda
import Util

newEnv :: Env
newEnv = Env M.empty

extend :: Env -> Ident -> Lam -> Env
extend (Env map) id x = Env (M.insert id x map)

-- Error if there is a name conflict.
combineNoClash :: Env -> Env -> Env
combineNoClash (Env m0) (Env m1) = Env $ M.unionWithKey noClash m0 m1
  where noClash k _ _ = error $ "Env clash on " ++ k

elookup :: Env -> Ident -> Maybe Lam
elookup (Env map) x = M.lookup x map
