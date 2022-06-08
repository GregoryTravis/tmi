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
import TMI
import Ty hiding (V, W)
import qualified Ty (V, W(..))
import Util
import V
import VReadShow
import W

-- start boilerplate
type W = Ty.W App
type V = Ty.V W
-- type Call = Ty.Call W
deriving instance Show W
deriving instance Read W

vroot :: V W
vroot = VRoot
-- end boilerplate

theWorld :: W
theWorld = Ty.W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall (k 13)]
                     , logWha = [vACallCPS (k 13)]
                     , logEvents = [RetVal "12"] }
data App = App {}
  deriving (Eq, Ord, Read, Show)

instance HasRecon W where
  getRecon "aCall" = unsafeCoerce $ VNamed "aCall" aCall

vwApp = fwApp vroot
vwSys = fwSys vroot
vsysLog = fsysLog vwSys
vlogCalls = flogCalls vsysLog
vlogWha = flogWha vsysLog
vlogEvents = flogEvents vsysLog

grabCall :: V (TMI W ())
grabCall = deref (vlogCalls !!. (VNice 0))

grabCallCPS :: V (CPS W ())
grabCallCPS = deref (vlogWha !!. (VNice 0))

grabEvent :: V Event
grabEvent = vlogEvents !!. (VNice 0)

aCall :: Int -> TMI w ()
aCall n = Bind (Step (Ret n)) (\_ -> Step (Ret ()))

vACall :: V Int -> V (TMI W ())
vACall = lift1 $ nuni "aCall" aCall

aCallCPS :: Int -> CPS w ()
aCallCPS n = cps (aCall n)
--aCallCPS n = Bind (Step (Ret n)) (\_ -> Step (Ret ()))

vACallCPS :: V Int -> V (CPS W ())
vACallCPS = lift1 $ nuni "aCallCPS" aCallCPS

oldMain = do
  let event = rd theWorld grabEvent
      call = rd theWorld grabCall
  let tmi = resolveCall call event
  let tmi' = rd theWorld $ vresolveCall grabCall grabEvent

  msp theWorld
  let tws = show theWorld
      tw = read tws :: W
  let tmi'' = rd tw $ vresolveCall grabCall grabEvent
  let cpstmi'' = rd tw $ vresolveCallCPS grabCallCPS grabEvent

  -- works
  -- msp vroot
  -- msp (show vroot)
  -- msp ((read (show vroot)) :: V W W)
  -- msp theWorld
  msp "hi log"
