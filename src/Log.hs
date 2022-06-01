{-# Language ExistentialQuantification, NamedFieldPuns #-}

module Log
( logMain ) where

import Unsafe.Coerce

import Storage
import Ty
import V
import Util

data App = App {}
  deriving (Eq, Ord, Read, Show)

data Log = Log
  { logCalls :: [V W Call]
  , logEvents :: [Event]
  }
  -- deriving (Eq, Ord, Read, Show)

data Sys = Sys { sysLog :: Log }
  -- deriving (Eq, Ord, Read, Show)

data W = W { wApp :: App, wSys :: Sys }
  -- deriving (Eq, Ord, Read, Show)

data TMI a = TMI a

data Call = forall a. Call (IO a) (a -> TMI ())

data Event = RetVal String -- | Command
  deriving (Eq, Ord, Read, Show)

anExt :: IO Int
anExt = return 12

aCont :: Int -> TMI ()
aCont x = TMI ()

aCall :: Call
aCall = Call anExt aCont

vACall :: V W Call
vACall = VNamed "aCall" aCall

recon :: String -> a
recon "aCall" = unsafeCoerce $ VNamed "aCall" aCall

instance Show (V w a) where
  show v = show (qs v)

instance Read (V w a) where
  readsPrec i s = readsPrecer recon i s

theWorld :: W
theWorld = W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall]
                     , logEvents = [] }

vroot :: V W W
vroot = VRoot

logMain = do
  msp vroot
  msp (show vroot)
  msp ((read (show vroot)) :: V W W)
  msp "hi log"
