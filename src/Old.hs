{-# Language NamedFieldPuns #-}

module Old (oldMain) where

import Data.List ((\\))

import Core
import Lens
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
  { invitees = ["a@b.com", "c@d.com"]
  , invited = ["c@d.com"]
  }

binvitees = field root "invitees" invitees $ \w invitees -> w { invitees }
binvited = field root "invited" invited $ \w invited -> w { invited }

bListDiff :: Eq a => V [a] -> V [a] -> V [a]
bListDiff = lift2 $ nuni "eq" (\\)

notYetInvited :: V [String]
notYetInvited = bListDiff binvitees binvited

lookupCommand :: AppEnv W
lookupCommand ["old"] = old

old :: Program W
old = toProg sdone old'

old' :: Blef W ()
old' = do
  monitor notYetInvited $ \x -> msp ("monny", x)
  io $ msp "hi old'"
  binvs <- BRead binvitees
  BWrite binvitees (binvs ++ ["e@f.com"])
  io $ msp "hi old'"
  return ()

oldApp = App { initialW = theWorld, appEnv = lookupCommand }

oldMain = do
  fromTheTop "old" oldApp ["old"]
