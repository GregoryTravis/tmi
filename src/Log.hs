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

type WW = W App

data Log w = Log
  { logCalls :: [V w (Call w)]
  , logEvents :: [Event]
  }
  deriving (Read, Show)

data Sys w = Sys { sysLog :: Log w }
  deriving (Read, Show)

data W app = W { wApp :: app, wSys :: Sys (W app) }
  deriving (Read, Show)

data TMI w a = TMI a

data Call w = forall a. Call (IO a) (a -> TMI w ())

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
  msp vroot
  msp (show vroot)
  msp ((read (show vroot)) :: V WW WW)
  msp theWorld
  msp "hi log"
