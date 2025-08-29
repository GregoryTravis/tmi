module Builtin
( BuiltinDefs(..)
, lyft1
, lyft2
, newBuiltins
, toBuiltinLam ) where

import Util
import Val

import qualified Data.Map.Strict as M

newBuiltins :: BuiltinDefs
newBuiltins = BuiltinDefs M.empty

toBuiltinLam :: BuiltinDef -> Code
toBuiltinLam (BuiltinDef id arity _) = Builtin id arity

lyft1 :: (a -> b) -> (Val -> a) -> (b -> Val) -> ([Val] -> Val)
lyft1 f ina outb [x] = outb (f (ina x))

lyft2 :: (a -> b -> c) -> (Val -> a) -> (Val -> b) -> (c -> Val) -> ([Val] -> Val)
lyft2 f ina inb outc [x, y] = outc (f (ina x) (inb y))
