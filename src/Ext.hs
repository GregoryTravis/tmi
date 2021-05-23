{-# LANGUAGE ExistentialQuantification, NumericUnderscores #-}

module Ext
( extMain
) where

import Control.Monad.State hiding (lift)

import Tmi
import Util

data W = W
  { invitedUsers :: [String] }
  deriving Show

world = W { invitedUsers = [] }

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
  invitedUsersV <--- VConst ["a"]

extMain = do
  (a, history') <- tmiRun history action
  msp a
  msp history'
  msp "ext hi"
