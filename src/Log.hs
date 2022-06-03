{-# Language ExistentialQuantification, NamedFieldPuns #-}

module Log
( flogCalls
, flogEvents
, resolveCall
, vresolveCall ) where

import Lens
import Lift
import Ty
import V
import Util

flogCalls :: V w (Log w) -> V w [V w (Call w)]
flogCalls log = field log "logCalls" logCalls $ \w logCalls -> w { logCalls }

flogEvents :: V w (Log w) -> V w [Event]
flogEvents log = field log "logEvents" logEvents $ \w logEvents -> w { logEvents }

resolveCall :: Call w -> Event -> TMI w ()
resolveCall (Call _ k) (RetVal eventString) = k (read eventString)

vresolveCall :: V w1 (Call w2) -> V w1 Event -> V w1 (TMI w2 ())
vresolveCall = lift2 $ nuni "resolveCall" resolveCall
