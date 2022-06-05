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
import Ty hiding (V)
import qualified Ty (V)
import Util
import V
import VReadShow
import W

-- start boilerplate
type WW = W App
type V = Ty.V WW
deriving instance Show WW
deriving instance Read WW

vroot :: V WW
vroot = VRoot
-- end boilerplate

theWorld :: WW
theWorld = W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall (k 13)]
                     , logEvents = [RetVal "12"] }
data App = App {}
  deriving (Eq, Ord, Read, Show)

instance HasRecon WW where
  getRecon "aCall" = unsafeCoerce $ VNamed "aCall" aCall

vwApp = fwApp vroot
vwSys = fwSys vroot
vsysLog = fsysLog vwSys
vlogCalls = flogCalls vsysLog
vlogEvents = flogEvents vsysLog

grabCall :: V (Call (W App))
grabCall = deref (vlogCalls !!. (VNice 0))

grabEvent :: V Event
grabEvent = vlogEvents !!. (VNice 0)

aCall :: Int -> Call WW
aCall n = Call (return n) (\_ -> TMI ())

vACall :: V Int -> V (Call WW)
vACall = lift1 $ nuni "aCall" aCall

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
