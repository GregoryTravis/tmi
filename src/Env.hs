module Env
( newEnv
, extend
, lookup ) where

import qualified Data.Map.Strict as M

import Lambda
import Util

newEnv :: Env
newEnv = M.empty

extend :: Env -> Ident -> Lam -> Env
extend (Env map) id x = Env (M.insert id x map)

-- Error if there is a name conflict.
combineNoClash :: Env -> Env -> Env
combineNoClase (Env m0) (Env m1) = Env $ unionWithKey noClash m0 m1
  where noClash k _ _ _ = error $ "Env clash on " ++ k

lookup :: Env -> Ident -> Maybe Value
lookup (Env map) x = M.lookup x map
