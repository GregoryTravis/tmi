{-# LANGUAGE ExistentialQuantification, NumericUnderscores #-}

module Ext
( extMain
) where

-- import Data.Dynamic
import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Data.Typeable
import Control.Monad.State hiding (lift)

import Tmi
import Util

data Request a = Request (IO a)

data W = W
  { invitedUsers :: [String]
  }
  deriving Show

world = W
  { invitedUsers = []
  }

history :: History W
history = mkHistory world

vw :: V W
vw = getRoot history

_invitedUsers :: V (R W -> R [String])
_invitedUsers = VConst __invitedUsers
  where __invitedUsers (R w rw) = (R i ri)
          where i = invitedUsers w
                ri = Receiver "_invitedUsers" $ \newI ->
                    rw <-- w { invitedUsers = newI }

invitedUsersV = _invitedUsers <$$> vw

listeny :: Show a => a -> IO ()
listeny x = putStrLn $ "Listeny: " ++ (show x)

action :: StateT (TmiState W) IO ()
action = do
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV listeny
  invitedUsersV <--- VConst ["b"]

modString_for :: String -> String
modString_for = (++ "!")
modString_rev :: R String -> String -> Write
modString_rev (R _ rs) newS =
  rs <-- init newS
modStringV :: V (R String -> R String)
modStringV = VConst $ hybrid1 modString_for modString_rev

-- requests = map toRequest invitedUsersV
-- duped = mapV modStringV invitedUsersV
-- mapV :: V (R String -> R String) -> V [String] -> V [String]
-- mapV :: V (R a -> R a) -> V [a] -> V [a]
-- mapV :: V (R (a -> b) -> R [a] -> R [b])
-- mapV = undefined

map_for :: (String -> String) -> [String] -> [String]
map_for = map
map_rev :: R (String -> String) -> R [String] -> [String] -> Write
map_rev = undefined
map_hy :: R (String -> String) -> R [String] -> R [String]
map_hy = hybrid2 map_for map_rev

extMain = do
  (a, history') <- tmiRun history action
  msp a
  msp history'
  -- msp $ typeOf map_hy
  msp "ext hi"

-- $> :t invitedUsersV
