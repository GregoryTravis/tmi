module Env
( newEnv
, startEnv
, extend
, combineNoClash
, elookup ) where

import qualified Data.Map.Strict as M

import Util
import Val

newEnv :: Env
newEnv = Env M.empty

startEnv :: Ident -> Val -> Env
startEnv id x = extend newEnv id x

extend :: Env -> Ident -> Val -> Env
extend (Env map) id x = Env (M.insert id x map)

instance Semigroup Env where
  Env a <> Env b = Env (b `M.union` a)

instance Monoid Env where
  mempty = newEnv

-- Error if there is a name conflict.
combineNoClash :: Env -> Env -> Env
combineNoClash (Env m0) (Env m1) = Env $ M.unionWithKey noClash m0 m1
  where noClash k _ _ = error $ "Env clash on " ++ k

elookup :: Env -> Ident -> Maybe Val
elookup (Env map) x = M.lookup x map
