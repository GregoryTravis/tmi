module Adt
( mkCtor ) where

import Builtin
import Util
import Val

mkCtor :: Ident -> Int -> Code
mkCtor name arity =
  let vars = map (("x" ++) . show) [0..arity-1]
      wrap [] = Ctor name (map Id vars)
      wrap (v:vs) = Lam v (wrap vs)
   in wrap vars
