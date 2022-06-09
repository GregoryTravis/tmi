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
deriving instance Show W
deriving instance Read W

vroot :: V W
vroot = VRoot
-- end boilerplate

theWorld :: W
theWorld = Ty.W
  { wApp = App { anInt = 12 }
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCPSs = [vaCPS (k 13)]
                     , logEvents = [RetVal "12"] }
data App = App
  { anInt :: Int
  }
  deriving (Eq, Ord, Read, Show)
fanInt app = field app "anInt" anInt $ \w anInt -> w { anInt }
vanInt = fanInt vwApp

instance HasRecon W where
  getRecon "aCPS" = unsafeCoerce $ VNamed "aCPS" aCPS

vwApp = fwApp vroot
vwSys = fwSys vroot
vsysLog = fsysLog vwSys
vlogCPSs = flogCPSs vsysLog
vlogEvents = flogEvents vsysLog

grabCPS :: V (CPS W ())
grabCPS = deref (vlogCPSs !!. (VNice 0))

grabEvent :: V Event
grabEvent = vlogEvents !!. (VNice 0)

aCPS :: Int -> CPS w ()
aCPS n = cps $ Bind (Step (Ret n)) (\_ -> Step (Ret ()))

vaCPS :: V Int -> V (CPS W ())
vaCPS = lift1 $ nuni "aCPS" aCPS

oldMain = do
  -- works
  -- let i = rd theWorld vanInt
  -- msp i
  -- let write = Write vanInt 14
  --     w' = propWrite theWorld write
  -- msp theWorld
  -- msp w'

  -- works
  let event = rd theWorld grabEvent
      cps = rd theWorld grabCPS
  let tmi = resolveCPS cps event
  let tmi' = rd theWorld $ vresolveCPS grabCPS grabEvent

  msp theWorld
  let tws = show theWorld
      tw = read tws :: W
  let tmi'' = rd tw $ vresolveCPS grabCPS grabEvent
  let cpstmi'' = rd tw $ vresolveCPS grabCPS grabEvent

  -- works
  -- msp vroot
  -- msp (show vroot)
  -- msp ((read (show vroot)) :: V W W)
  -- msp theWorld
  msp "hi log"
