{-# Language NamedFieldPuns #-}

module Old (oldMain) where

import Core
import Lift
import MainLoop
import Monad
import Propagate
import Ty hiding (V, Bi, R)
import qualified Ty as Ty
import Util
import V
import Veq

type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W
root :: V W
root = VRoot

data W = W
  { invitees :: [String]
  , invited :: [String]
  }
  deriving (Eq, Read, Show)

theWorld :: W
theWorld = W
  { invitees = []
  , invited = []
  }

binvitees :: V [String]
binvitees = VBiSeal (BiApp (bi (VNamed "invitees" invitees) (VNamed "invitees_" invitees_)) root)
invitees_ :: W -> R W -> R [String]
invitees_ w wr = mkR ir
  where ir invitees = write wr $ w { invitees }

lookupCommand :: AppEnv W
lookupCommand ["old"] = old

old :: Program W
old = toProg sdone old'

old' :: Blef w ()
old' = do
  io $ msp "hi old'"
  return ()

oldApp = App { initialW = theWorld, appEnv = lookupCommand }

oldMain = do
  fromTheTop "old" oldApp ["old"]
