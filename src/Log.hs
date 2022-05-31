{-# Language ExistentialQuantification, NamedFieldPuns #-}

module Log
( logMain ) where

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
vACall = VNamed "vACall" aCall

theWorld :: W
theWorld = W
  { wApp = App {}
  , wSys = Sys { sysLog } }
  where sysLog = Log { logCalls = [vACall]
                     , logEvents = [] }

logMain = do
  -- let s = qs theWorld
  -- msp s
  msp "hi log"
