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
  , aList :: [Int]
  }
  deriving Show

world = W
  { invitedUsers = []
  , aList = [30, 40, 50]
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

_aList :: V (R W -> R [Int])
_aList = VConst __aList
  where __aList (R w rw) = (R i ri)
          where i = aList w
                ri = Receiver "_aList" $ \newI ->
                    rw <-- w { aList = newI }

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
  let -- foo :: [a] -> [b] -> Write
      -- TODO not have to reverse this?
      foo nas [] [] = ras (reverse nas)
      foo nas (oa:oas) (b:bs) =
        let ra = R oa (Receiver "map_rev" cont)
            cont na = foo (na:nas) oas bs
         in rev ra b
      -- rev :: R a -> b -> Write
      rev = hyGetRev1 rf
   in foo [] oas bs
mapHy :: R (R a -> R b) -> R [a] -> R [b]
mapHy = hybrid2 map_for map_rev
mapV :: V (R (R a -> R b) -> R [a] -> R [b])
mapV = VConst mapHy

-- -- External map
-- -- Just writing it in regular and primitive forms
-- mapE :: (a -> b) -> [a] -> [b]
-- mapE _ [] = []
-- mapE f (x:xs) = f x : (mapE f xs)
-- mapE' :: (a -> b) -> [a] -> [b]
-- mapE' f xs =
--   if null xs
--     then []
--     else (f (head xs)) : (map f (tail xs))

-- -- External
-- mapVE :: V (a -> b) -> V [a] -> V [b]
-- mapVE vf vas =
--   ifV <**> (

ifV_for :: Bool -> a -> a -> a
ifV_for b t e = if b then t else e
ifV_rev :: R Bool -> R a -> R a -> a -> Write
ifV_rev = undefined
ifV :: V (R Bool -> R a -> R a -> R a)
ifV = VConst $ hybrid3 ifV_for ifV_rev

headR :: R [a] -> R a
headR (R as (Receiver _ ras)) = R a ra
  where a = head as
        ra = Receiver "headR" $ \a -> ras (a : tail as)
headV :: V (R [a] -> R a)
headV = VConst headR

tailR :: R [a] -> R [a]
tailR (R as (Receiver _ ras)) = R as' ras'
  where as' = tail as
        ras' = Receiver "tailR" $ \as' -> ras (head as : as')
tailV :: V (R [a] -> R [a])
tailV = VConst tailR

consR :: R a -> R [a] -> R [a]
consR (R a ra) (R as ras) = R as' ras'
  where as' = a:as
        ras' = Receiver "consR" $ \(a':as') -> (ra <-- a') <> (ras <-- as')
consV = VConst consR

nullR :: R [a] -> R Bool
nullR = hybrid1 null undefined

nullV :: V (R [a] -> R Bool)
nullV = VConst nullR

-- (<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
modded = mapV <**> modStringV <$$> invitedUsersV

action :: StateT (TmiState W) IO ()
action = do
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV listeny
  listen modded listeny
  listen (ifV <**> VConst True <**> VConst 2 <$$> VConst 3) listeny
  listen (ifV <**> VConst False <**> VConst 2 <$$> VConst 3) listeny
  listen (headV <$$> VConst [3, 4, 5]) listeny
  listen (tailV <$$> VConst [3, 4, 5]) listeny
  listen (_aList <$$> vw) listeny
  listen (headV <$$> (_aList <$$> vw)) listeny
  listen (tailV <$$> (_aList <$$> vw)) listeny
  listen (consV <**> (headV <$$> (_aList <$$> vw))
                <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) listeny
  invitedUsersV <--- VConst ["b", "heyo", "hippo"]
  modded <--- VConst ["c!", "deyo!", "lippo!"]
  (headV <$$> (_aList <$$> vw)) <--- VConst 31
  (tailV <$$> (_aList <$$> vw)) <--- VConst [42, 52]
  -- Non-singular write
  -- (consV <**> (headV <$$> (_aList <$$> vw))
  --        <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) <--- VConst [310, 520]

extMain = do
  (a, history') <- tmiRun history action
  msp a
  msp history'
  runListeners history'
  msp $ case history' of History _ listeners -> length listeners
  -- msp $ map (+1) [3, 4, 5]
  -- msp $ mapE (+1) [3, 4, 5]
  -- msp $ mapE' (+1) [3, 4, 5]
  msp "ext hi"

-- $> :t invitedUsersV
