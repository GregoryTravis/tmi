{-# Language ExistentialQuantification, NamedFieldPuns #-}

module Log
( logMain ) where

import Unsafe.Coerce

import Lens
import Lift
import Propagate
import Storage
import Ty
import V
import Util

data App = App {}
  deriving (Eq, Ord, Read, Show)

type WW = W App

data Log w = Log
  { logCalls :: [V w (Call w)]
  , logEvents :: [Event]
  }
  deriving (Read, Show)
vlogCalls :: V WW [V (W App) (Call (W App))]
vlogCalls = field vsysLog "logCalls" logCalls $ \w logCalls -> w { logCalls }
vlogEvents = field vsysLog "logEvents" logEvents $ \w logEvents -> w { logEvents }

grabCall :: V (W App) (Call (W App))
grabCall = deref (vlogCalls !!. (VNice 0))

grabEvent :: V WW Event
grabEvent = vlogEvents !!. (VNice 0)

(!!.) :: V w [a] -> V w Int -> V w a
(!!.) = lift2 $ nuni "!!." (!!)

data Sys w = Sys { sysLog :: Log w }
  deriving (Read, Show)
vsysLog = field vwSys "sysLog" sysLog $ \w sysLog -> w { sysLog }

data W app = W { wApp :: app, wSys :: Sys (W app) }
  deriving (Read, Show)
vwApp = field vroot "wApp" wApp $ \w wApp -> w { wApp }
vwSys = field vroot "wSys" wSys $ \w wSys -> w { wSys }

data TMI w a = TMI a
  deriving (Eq, Ord, Read, Show)

data Call w = forall a. (Read a, Show a) => Call (IO a) (a -> TMI w ())
-- vcallK :: V w (Call w) -> V w (a -> TMI w ())
-- vcallK = lift1 $ nuni "callK" (\(Call _ k) -> k)

resolveCall :: Call w -> Event -> TMI w ()
resolveCall (Call _ k) (RetVal eventString) = k (read eventString)

vresolveCall :: V w1 (Call w2) -> V w1 Event -> V w1 (TMI w2 ())
vresolveCall = lift2 $ nuni "resolveCall" resolveCall

data Event = RetVal String -- | Command
  deriving (Eq, Ord, Read, Show)

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

-- TODO: pull recon from a typeclass implemented by Ws and move this to V
instance Show (V w a) where
  show v = show (qs v)

instance Read (V w a) where
  readsPrec i s = readsPrecer recon i s

theWorld :: WW
theWorld = W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall]
                     , logEvents = [RetVal "12"] }

vroot :: V WW WW
vroot = VRoot

logMain = do
  let event = rd theWorld grabEvent
      call = rd theWorld grabCall
  msp $ resolveCall call event

  -- works
  -- msp vroot
  -- msp (show vroot)
  -- msp ((read (show vroot)) :: V WW WW)
  -- msp theWorld
  msp "hi log"
