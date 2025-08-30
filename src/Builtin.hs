module Builtin
( BuiltinDefs(..)
, lyft1
, lyft2
, wrapBuiltin ) where

import Util
import Val

import qualified Data.Map.Strict as M

wrapBuiltin :: BuiltinDef -> Code
wrapBuiltin (BuiltinDef name arity _) =
  let vars = map (("x" ++) . show) [0..arity-1]
      wrap [] = Builtin name (map Id vars)
      wrap (v:vs) = Lam v (wrap vs)
   in wrap vars

lyft1 :: (a -> b) -> (Val -> a) -> (b -> Val) -> ([Val] -> Val)
lyft1 f ina outb [x] = outb (f (ina x))

lyft2 :: (a -> b -> c) -> (Val -> a) -> (Val -> b) -> (c -> Val) -> ([Val] -> Val)
lyft2 f ina inb outc [x, y] = outc (f (ina x) (inb y))
