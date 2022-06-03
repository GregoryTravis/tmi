{-# Language AllowAmbiguousTypes, FlexibleInstances, NamedFieldPuns, StandaloneDeriving, ScopedTypeVariables,
             TypeApplications, UndecidableInstances #-}

module Old
( oldMain ) where

import Unsafe.Coerce

import Lens
import Lib
import Log
import Propagate
import Storage
import Sys
import Ty
import Util
import V
import W

data App = App {}
  deriving (Eq, Ord, Read, Show)

class HasRecon w where
  getRecon :: String -> a

instance HasRecon WW where
  getRecon = recon

type WW = W App

vlogCalls = flogCalls vsysLog
vlogEvents = flogEvents vsysLog

grabCall :: V (W App) (Call (W App))
grabCall = deref (vlogCalls !!. (VNice 0))

grabEvent :: V WW Event
grabEvent = vlogEvents !!. (VNice 0)

anExt :: IO Int
anExt = return 12

aCont :: Int -> TMI WW ()
aCont x = TMI ()

aCall :: Call WW
aCall = Call anExt aCont

vACall :: V WW (Call WW)
vACall = VNamed "aCall" aCall

recon :: String -> a
recon "aCall" = unsafeCoerce $ VNamed "aCall" aCall

-- deriving instance (Read, Show) Log
deriving instance HasRecon w => Read (Log w)
deriving instance HasRecon w => Show (Log w)
deriving instance HasRecon w => Read (Sys w)
deriving instance HasRecon w => Show (Sys w)
deriving instance Show WW
deriving instance Read WW

-- TODO: pull recon from a typeclass implemented by Ws and move this to V
instance Show (V w a) where
  show v = show (qs v)

instance HasRecon w => Read (V w a) where
  readsPrec i s = readsPrecer (getRecon @w) i s

vwApp = fwApp vroot
vwSys = fwSys vroot
vsysLog = fsysLog vwSys

theWorld :: WW
theWorld = W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall]
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
