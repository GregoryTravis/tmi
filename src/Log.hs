{-# Language ExistentialQuantification #-}

module Log
( resolveCall
, vresolveCall ) where

import Lift
import Ty
import V
import Util

resolveCall :: Call w -> Event -> TMI w ()
resolveCall (Call _ k) (RetVal eventString) = k (read eventString)

vresolveCall :: V w1 (Call w2) -> V w1 Event -> V w1 (TMI w2 ())
vresolveCall = lift2 $ nuni "resolveCall" resolveCall
