{-# Language ExistentialQuantification, NamedFieldPuns #-}

module Log
( flogCalls
, flogWha
, flogEvents
, resolveCall
, vresolveCall
, vresolveCallCPS ) where

import Lens
import Lift
import Ty
import V
import Util

flogCalls :: V w (Log w) -> V w [V w (TMI w ())]
flogCalls log = field log "logCalls" logCalls $ \w logCalls -> w { logCalls }

flogWha :: V w (Log w) -> V w [V w (CPS w ())]
flogWha log = field log "logWha" logWha $ \w logWha -> w { logWha }

flogEvents :: V w (Log w) -> V w [Event]
flogEvents log = field log "logEvents" logEvents $ \w logEvents -> w { logEvents }

resolveCall :: TMI w () -> Event -> TMI w ()
resolveCall (Bind _ k) (RetVal eventString) = k (read eventString)

vresolveCall :: V w (TMI w ()) -> V w Event -> V w (TMI w ())
vresolveCall = lift2 $ nuni "resolveCall" resolveCall

resolveCallCPS :: CPS w () -> Event -> CPS w ()
resolveCallCPS (KBind _ k) (RetVal eventString) = k (read eventString)

vresolveCallCPS :: V w (CPS w ()) -> V w Event -> V w (CPS w ())
vresolveCallCPS = lift2 $ nuni "resolveCallCPS" resolveCallCPS
