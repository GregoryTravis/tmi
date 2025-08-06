module Builtin
( BuiltinDefs(..)
, addToEnv
, lyft1
, lyft2
, newBuiltins
, toBuiltinLam ) where

import Lambda
import Util

import qualified Data.Map.Strict as M

newBuiltins :: BuiltinDefs
newBuiltins = BuiltinDefs M.empty

toBuiltinLam :: BuiltinDef -> Lam
toBuiltinLam (BuiltinDef id arity _) = Builtin id arity

lyft1 :: (a -> b) -> (Lam -> a) -> (b -> Lam) -> (Lam -> Lam)
lyft1 f in out x = out (f (in x))

lyft2 :: (a -> b -> c) -> (Lam -> a) -> (Lam -> b) -> (c -> Lam) -> (Lam -> Lam -> Lam)
lyft2 f ina inb outc x y = outc (f (ina x) (inb y))
