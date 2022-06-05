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
import Ty hiding (V, W, Call)
import qualified Ty (V, W(..), Call(..))
import Util
import V
import VReadShow
import W

-- start boilerplate
type W = Ty.W App
type V = Ty.V W
type Call = Ty.Call W
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
                     , logEvents = [RetVal "12"] }
data App = App {}
  deriving (Eq, Ord, Read, Show)

instance HasRecon W where
  getRecon "aCall" = unsafeCoerce $ VNamed "aCall" aCall

vwApp = fwApp vroot
vwSys = fwSys vroot
vsysLog = fsysLog vwSys
vlogCalls = flogCalls vsysLog
vlogEvents = flogEvents vsysLog

grabCall :: V Call
grabCall = deref (vlogCalls !!. (VNice 0))

grabEvent :: V Event
grabEvent = vlogEvents !!. (VNice 0)

aCall :: Int -> Call
aCall n = Ty.Call (return n) (\_ -> TMI ())

vACall :: V Int -> V Call
vACall = lift1 $ nuni "aCall" aCall

oldMain = do
  let event = rd theWorld grabEvent
      call = rd theWorld grabCall
  msp $ resolveCall call event
  msp $ rd theWorld $ vresolveCall grabCall grabEvent

  msp theWorld
  let tws = show theWorld
      tw = read tws :: W
  msp $ rd tw $ vresolveCall grabCall grabEvent

  -- works
  -- msp vroot
  -- msp (show vroot)
  -- msp ((read (show vroot)) :: V W W)
  -- msp theWorld
  msp "hi log"
