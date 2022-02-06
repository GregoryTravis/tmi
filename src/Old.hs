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
  , joined :: [String]
  , declined :: [String]
  }
  deriving (Eq, Read, Show)

theWorld :: W
theWorld = W
  { invitees = ["a@b.com", "c@d.com"]
  , invited = ["c@d.com"]
  , joined = []
  , declined = []
  }

binvitees = field root "invitees" invitees $ \w invitees -> w { invitees }
binvited = field root "invited" invited $ \w invited -> w { invited }
bjoined = field root "joined" joined $ \w joined -> w { joined }
bdeclined = field root "declined" declined $ \w declined -> w { declined }

bListDiff :: Eq a => V [a] -> V [a] -> V [a]
bListDiff = lift2 $ nuni "eq" (\\)

notYetInvited :: V [String]
notYetInvited = bListDiff binvitees binvited

lookupCommand :: AppEnv W
lookupCommand ["old"] = old

vappend :: Ty.V w [a] -> a -> Blef w ()
vappend vas a = do
  as <- BRead vas
  BWrite vas (as ++ [a])

old :: Program W
old = toProg sdone old'

old' :: Blef W ()
old' = do
  monitor notYetInvited $ \x -> msp ("nyi", x)
  monitor bjoined $ \x -> msp ("joined", x)
  monitor bdeclined $ \x -> msp ("declined", x)
  io $ msp "hi old'"
  -- binvs <- BRead binvitees
  -- BWrite binvitees (binvs ++ ["e@f.com"])
  vappend binvitees "e@f.com"
  io $ msp "hi old'"
  inviteTheUninvited
  return ()

data RSVP = Accept | Reject deriving (Read, Show)

inviteTheUninvited :: Blef W ()
inviteTheUninvited = do
  nyi <- BRead notYetInvited
  let one = head nyi
  rsvp <- EBlef "rsvp" $ \h -> do
    let text = "sent to " ++ one ++ "\n" ++
               "accept: " ++ acceptToken ++ "\n" ++
               "reject: " ++ rejectToken ++ "\n"
        acceptToken = show (Retval h "Accept")
        rejectToken = show (Retval h "Reject")
    msp text
  case rsvp of Accept -> vappend bjoined one
               Reject -> vappend bdeclined one
  io $ msp $ "gonna email" ++ (show nyi)

oldApp = App { initialW = theWorld, appEnv = lookupCommand }

oldMain = do
  let dbdir = "old"
  fromTheTop dbdir oldApp ["old"]
  -- injectEvent dbdir oldApp (Retval 2 "Accept")
  injectEvent dbdir oldApp (Retval 2 "Reject")
  run oldApp dbdir
