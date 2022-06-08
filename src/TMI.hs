module TMI
( cps ) where

import Ty
import Util

cps :: (Read a, Show a) => TMI w a -> CPS w a
cps tmi = cps' tmi (\_ -> Done)

cps' :: (Read a, Show a) => TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
cps' (Bind (Step a) k') k = KBind a (\a -> cps' (k' a) k)
