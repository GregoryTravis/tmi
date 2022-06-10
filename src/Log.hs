{-# Language ExistentialQuantification, NamedFieldPuns #-}

module Log
( flogCPSs
, flogEvents
, resolveCPS
, vresolveCPS ) where

import Lens
import Lift
import Ty
import V
import Util

flogCPSs :: V w (Log w) -> V w [V w (CPS w ())]
flogCPSs log = field log "logCPSs" logCPSs $ \w logCPSs -> w { logCPSs }

flogEvents :: V w (Log w) -> V w [Event]
flogEvents log = field log "logEvents" logEvents $ \w logEvents -> w { logEvents }

resolveCPS :: CPS w () -> Event -> CPS w ()
resolveCPS (KBind (Ext _) k) (RetVal eventString) = k (read eventString)
resolveCPS _ e = error $ "Can't resolve: " ++ show e

vresolveCPS :: V w (CPS w ()) -> V w Event -> V w (CPS w ())
vresolveCPS = lift2 $ nuni "resolveCPS" resolveCPS
