{-# Language NamedFieldPuns #-}

module Old (oldMain) where

import Data.List ((\\))

import Alloc
import Core
import Lens
import Lift
import MainLoop
import Monad
import Parr
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
  , allocator :: Alloc
  }
  deriving (Read, Show)

theWorld :: W
theWorld = W
  { invitees = ["a@b.com", "c@d.com", "g@h.com"]
  , invited = ["c@d.com"]
  , joined = []
  , declined = []
  , allocator = mkAlloc
  }

binvitees = field root "invitees" invitees $ \w invitees -> w { invitees }
binvited = field root "invited" invited $ \w invited -> w { invited }
bjoined = field root "joined" joined $ \w joined -> w { joined }
bdeclined = field root "declined" declined $ \w declined -> w { declined }
ballocator = field root "allocator" allocator $ \w allocator -> w { allocator }

bListDiff :: Eq a => V [a] -> V [a] -> V [a]
bListDiff = lift2 $ nuni "eq" (\\)

(++.) :: Ty.V w [a] -> Ty.V w [a] -> Ty.V w [a]
(++.) = lift2 $ nuni "append" (++)

notYetInvited :: V [String]
notYetInvited = bListDiff binvitees binvited

lookupCommand :: AppEnv W
lookupCommand ["inviteToSite"] = inviteToSite
lookupCommand ["inviteToGame"] = inviteToGameP ["a@b.com", "e@f.com", "g@h.com"]

-- TODO don't want this Show a
vappend :: Show a => Ty.V w [a] -> a -> Blef w ()
vappend vas a = vas <--+ (++. (k [a]))

inviteToSite :: Program W
inviteToSite = toProg sdone inviteToSite'

inviteToSite' :: Blef W ()
inviteToSite' = do
  monitor notYetInvited $ \x -> msp ("nyi", x)
  monitor bjoined $ \x -> msp ("joined", x)
  monitor bdeclined $ \x -> msp ("declined", x)
  monitor binvited $ \x -> msp ("invited", x)
  io $ msp "hi old'"
  -- binvs <- BRead binvitees
  -- BWrite binvitees (binvs ++ ["e@f.com"])
  binvitees <--+ (++. k ["e@f.com"])
  io $ msp "hi old'"
  inviteTheUninvited
  return ()

data RSVP = Accept | Reject deriving (Eq, Read, Show)

inviteToGameP :: [String] -> Program W
inviteToGameP emails = toProg sdone (inviteToGame emails)

-- TODO should confirm they are in the user list
inviteToGame :: [String] -> Blef W ()
inviteToGame emails = do
  results <- parrList ballocator (map invitePlayerToGame emails)
  if all (==Accept) results
    then startGame emails
    else io $ msp $ "Someone doesn't want to play: " ++ (show $ zip emails results)

invitePlayerToGame :: String -> Blef W RSVP
invitePlayerToGame email = do
  EBlef "game rsvp" $ \h -> do
    let text = "sent to " ++ email ++ "\n" ++
               "accept: " ++ acceptToken ++ "\n" ++
               "reject: " ++ rejectToken ++ "\n"
        acceptToken = show (Retval h "Accept")
        rejectToken = show (Retval h "Reject")
    msp text

startGame :: [String] -> Blef W ()
startGame emails = io $ msp $ "starting game with " ++ show emails

invitePlayer :: String -> Blef W ()
invitePlayer email = do
  binvited <--+ (++. k [email])
  rsvp <- EBlef "rsvp" $ \h -> do
    let text = "sent to " ++ email ++ "\n" ++
               "accept: " ++ acceptToken ++ "\n" ++
               "reject: " ++ rejectToken ++ "\n"
        acceptToken = show (Retval h "Accept")
        rejectToken = show (Retval h "Reject")
    msp text
  case rsvp of Accept -> bjoined <--+ (++. k [email])
               Reject -> bdeclined <--+ (++. k [email])

inviteTheUninvited :: Blef W ()
inviteTheUninvited = do
  nyi <- BRead notYetInvited
  mapM_ (BFork . invitePlayer) nyi
  io $ msp $ "gonna email " ++ (show nyi)

oldApp = App { initialW = theWorld, appEnv = lookupCommand }

oldMain = do
  let dbdir = "old"

  fromTheTop dbdir oldApp ["inviteToGame"]
  injectEvent dbdir oldApp (Retval 0 "Accept")
  injectEvent dbdir oldApp (Retval 1 "Accept")
  injectEvent dbdir oldApp (Retval 2 "Reject")
  run oldApp dbdir

  -- fromTheTop dbdir oldApp ["inviteToSite"]
  -- injectEvent dbdir oldApp (Retval 2 "Reject")
  -- injectEvent dbdir oldApp (Retval 3 "Accept")
  -- injectEvent dbdir oldApp (Retval 4 "Reject")
  -- run oldApp dbdir
