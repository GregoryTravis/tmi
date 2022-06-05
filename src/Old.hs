{-# Language FlexibleInstances, NamedFieldPuns, StandaloneDeriving, ScopedTypeVariables,
             TypeApplications #-}

module Old
( oldMain ) where

import Unsafe.Coerce

import Lens
import Lift
import Lib
import Log
import Propagate
import Recon
import Storage
import Sys
import Ty
import Util
import V
import VReadShow
import W

data App = App {}
  deriving (Eq, Ord, Read, Show)

instance HasRecon WW where
  getRecon = recon

type WW = W App

vlogCalls = flogCalls vsysLog
vlogEvents = flogEvents vsysLog

grabCall :: V (W App) (Call (W App))
grabCall = deref (vlogCalls !!. (VNice 0))

grabEvent :: V WW Event
grabEvent = vlogEvents !!. (VNice 0)

-- anExt :: IO Int
-- anExt = return 12

-- aCont :: Int -> TMI WW ()
-- aCont x = TMI ()

aCall :: Int -> Call WW
aCall n = Call (return n) (\_ -> TMI ())

vACall :: V WW Int -> V WW (Call WW)
-- vACall = VNamed "aCall" aCall
vACall = lift1 $ nuni "aCall" aCall

recon :: String -> a
recon "aCall" = unsafeCoerce $ VNamed "aCall" aCall

-- deriving instance (Read, Show) Log
deriving instance HasRecon w => Read (Log w)
deriving instance HasRecon w => Show (Log w)
deriving instance HasRecon w => Read (Sys w)
deriving instance HasRecon w => Show (Sys w)
deriving instance Show WW
deriving instance Read WW

vwApp = fwApp vroot
vwSys = fwSys vroot
vsysLog = fsysLog vwSys

theWorld :: WW
theWorld = W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall (k 13)]
                     , logEvents = [RetVal "12"] }
vroot :: V WW WW
vroot = VRoot

oldMain = do
  let event = rd theWorld grabEvent
      call = rd theWorld grabCall
  msp $ resolveCall call event
  msp $ rd theWorld $ vresolveCall grabCall grabEvent

  msp theWorld
  let tws = show theWorld
      tw = read tws :: WW
  msp $ rd tw $ vresolveCall grabCall grabEvent

  -- works
  -- msp vroot
  -- msp (show vroot)
  -- msp ((read (show vroot)) :: V WW WW)
  -- msp theWorld
  msp "hi log"
