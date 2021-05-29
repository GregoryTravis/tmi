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

invitedUsersV :: V [String]
invitedUsersV = _invitedUsers <$$> vw

listeny :: Show a => a -> IO ()
listeny x = putStrLn $ "Listeny: " ++ (show x)

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

-- TODO this doesn't seem right
hyGetFor1 :: (R a -> R b) -> (a -> b)
hyGetFor1 rf a =
  case rf (R a (error "whoopsie hyGetFor1")) of
    R b _ -> b
hyGetRev1 :: (R a -> R b) -> (R a -> b -> Write)
hyGetRev1 rf ra =
  let (R _ (Receiver _ rb)) = rf ra
   in rb

map_for :: (R a -> R b) -> [a] -> [b]
map_for rf as = map (hyGetFor1 rf) as
map_rev :: R (R a -> R b) -> R [a] -> [b] -> Write
map_rev (R rf _) (R oas (Receiver _ ras)) bs =
  let ize = [0 .. length bs - 1]
      rev = hyGetRev1 rf
      writes = map wr ize
      wr i =
        let b = bs !! i
            oa = oas !! i
            r = R oa (Receiver "map_rev" rec)
            -- rec :: a -> Write
            rec a' = ras $ upd oas i a'
         in rev r b
   in foldr (<>) emptyWrite writes
-- map_rev (R rf _) (R oas ras) bs =
--   let ize = [0 .. length bs - 1]
--       rev = hyGetRev1 rf
--       nas = map wr ize
--       wr i =
--         let r = R oa (Receiver "map_rev" rec)
--             rec a = Write [Write1 (VConst undefined) a]
--             wrt = rev r b
--             na = case wrt of Write [Write1 _ na] -> na
--             oa = oas !! i
--             b = bs !! i
--          in na
--    in ras <-- nas
mapHy :: R (R a -> R b) -> R [a] -> R [b]
mapHy = hybrid2 map_for map_rev
mapV :: V (R (R a -> R b) -> R [a] -> R [b])
mapV = VConst mapHy

-- (<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
modded = mapV <**> modStringV <$$> invitedUsersV

action :: StateT (TmiState W) IO ()
action = do
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV listeny
  listen modded listeny
  invitedUsersV <--- VConst ["b", "heyo", "hippo"]
  -- modded <--- VConst ["c!", "deyo!", "lippo!"]

extMain = do
  (a, history') <- tmiRun history action
  msp a
  msp history'
  runListeners history'
  msp $ case history' of History _ listeners -> length listeners
  msp "ext hi"

-- $> :t invitedUsersV
