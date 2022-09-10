{-# Language FlexibleInstances, NamedFieldPuns, StandaloneDeriving, ScopedTypeVariables,
             TypeApplications #-}

module Old
( oldMain ) where

import Unsafe.Coerce

import H
import Lens
import Lift
import Lib
import Propagate
import Recon
import Runtime
import Storage
import TMI
import Ty hiding (V, H, TMI, CPS)
import qualified Ty (V, H(..), TMI(..), CPS(..))
import Util
import V
import VReadShow

-- start boilerplate
type V = Ty.V W
type H = Ty.H W
type TMI = Ty.TMI W
type CPS = Ty.CPS W
vroot :: V W
vroot = VRoot
-- end boilerplate

theWorld :: W
theWorld = W { anInt = 12 }

theHistory :: H
theHistory = initHistory vTheMain theWorld

data W = W
  { anInt :: Int
  }
  deriving (Eq, Ord, Read, Show)
fanInt app = field app "anInt" anInt $ \w anInt -> w { anInt }
vanInt = fanInt vroot

instance HasRecon W where
  getRecon _ = error "unimplemented"
  -- getRecon "aCPS" = unsafeCoerce $ VNamed "aCPS" aCPS

theMain :: TMI ()
-- theMain = Step (Ret ())
-- theMain = Step (Ext (msp "uff da"))
theMain = Bind (Step (Ext (readFile "asdf"))) (\s -> Step (Ext (msp $ "ooo " ++ s)))

vTheMain :: V (TMI ())
vTheMain = VNamed "theMain" theMain

oldMain :: IO ()
oldMain = do
  mainLoop theHistory
  msp "hi oldMain"
