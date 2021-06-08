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
  , anotherList :: [Int]
  , zero :: Int
  }
  deriving Show

world = W
  { invitedUsers = []
  , aList = [30, 40, 50]
  , anotherList = [2, 3, 4]
  , zero = 0
  }

history :: History W
history = mkHistory world

vw :: V W
vw = getRoot history

_invitedUsers :: V (R W -> R [String])
_invitedUsers = VConst "__invitedUsers" __invitedUsers
  where __invitedUsers (R w rw) = (R i ri)
          where i = invitedUsers w
                ri = Receiver "_invitedUsers" $ \newI ->
                    rw <-- w { invitedUsers = newI }

_aList :: V (R W -> R [Int])
_aList = VConst "__aList" __aList
  where __aList (R w rw) = (R i ri)
          where i = aList w
                ri = Receiver "_aList" $ \newI ->
                    rw <-- w { aList = newI }

_anotherList :: V (R W -> R [Int])
_anotherList = VConst "__anotherList" __anotherList
  where __anotherList (R w rw) = (R i ri)
          where i = anotherList w
                ri = Receiver "_anotherList" $ \newI ->
                    rw <-- w { anotherList = newI }

_zero :: V (R W -> R Int)
_zero = VConst "__zero" __zero
  where __zero (R w rw) = (R i ri)
          where i = zero w
                ri = Receiver "_zero" $ \newI ->
                    rw <-- w { zero = newI }

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
modStringV = VConst "modStringV" $ hybrid1 modString_for modString_rev

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
mapV = VConst "mapV" mapHy

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

-- External
mapVE :: (Show a, Show b) => V (R a -> R b) -> V [a] -> V [b]
mapVE vf vas =
  ifV <**> (nullV <$$> vas)
      <**> (VConst "[]" [])
      <$$> (consV <**> (vf <$$> (headV <$$> vas))
                  <$$> (mapVE vf
                         (tailV <$$> vas)))

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
fyold :: (a -> b -> b) -> b -> [a] -> b
fyold f b (a:as) = fyold f (f a b) as
fyold f b [] = b
fyold' f b as =
  if null as
    then b
    else fyold' f (f (head as) b) (tail as)

foldrVE :: (Show a, Show b) => V (R a -> R b -> R b) -> V b -> V [a] -> V b
foldrVE vf vb vas =
  ifV <**> (nullV <$$> vas)
      <**> vb
      <$$> (foldrVE vf
                    (vf <**> (headV <$$> vas) <$$> vb)
                    (tailV <$$> vas))

shiftNAdd :: R Int -> R Int -> R Int
shiftNAdd (R x rx) (R y ry) = R z rz
  where z = y * 10 + x
        rz = Receiver "shiftNAdd" $ \z' -> let x' = z' `mod` 10
                                               y' = z' `div` 10
                                            in (rx <-- x') <> (ry <-- y')
shiftNAddV :: V (R Int -> R Int -> R Int)
shiftNAddV = VConst "shiftNAddV" shiftNAdd

ifV_for :: Bool -> a -> a -> a
ifV_for b t e = if b then t else e
ifV_rev :: R Bool -> R a -> R a -> a -> Write
-- ifV_rev = error "ifV_rev"
-- ifV_rev _ _  _ _ = emptyWrite
ifV_rev (R b _rb) (R t rt) (R e re) x =
  let Receiver _ rec = if b then rt else re
   in rec x
ifV :: V (R Bool -> R a -> R a -> R a)
ifV = VConst "ifV" $ hybrid3 ifV_for ifV_rev

headR :: R [a] -> R a
headR (R as (Receiver _ ras)) = R a ra
  where a = head as
        ra = Receiver "headR" $ \a -> ras (a : tail as)
headV :: V (R [a] -> R a)
headV = VConst "headV" headR

tailR :: R [a] -> R [a]
tailR (R as (Receiver _ ras)) = R as' ras'
  where as' = tail as
        ras' = Receiver "tailR" $ \as' -> ras (head as : as')
tailV :: V (R [a] -> R [a])
tailV = VConst "tailV" tailR

consR :: R a -> R [a] -> R [a]
consR (R a ra) (R as ras) = R newAs ras'
  where newAs = a:as
        ras' = Receiver "consR" $ \(a':as') -> (ra <-- a') <> writeIfNotNil as'
        writeIfNotNil as' =
          if null as && null as'
            then emptyWrite
            else ras <-- as'
consV :: V (R a -> R [a] -> R [a])
consV = VConst "consV" consR

nullR :: R [a] -> R Bool
nullR = hybrid1 null undefined

nullV :: V (R [a] -> R Bool)
nullV = VConst "nullV" nullR

-- (<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
modded = mapV <**> modStringV <$$> invitedUsersV

inc_hy :: R Int -> R Int
inc_hy (R x rx) = R x' rx'
  where x' = x + 1
        rx' = Receiver "inc_hy" $ \x ->
          rx <-- (x - 1)
incV :: V (R Int -> R Int)
incV = VConst "incV" inc_hy

action :: StateT (TmiState W) IO ()
action = do
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV listeny
  listen modded listeny
  -- listen (ifV <**> VConst True <**> VConst 2 <$$> VConst 3) listeny
  -- listen (ifV <**> VConst False <**> VConst 2 <$$> VConst 3) listeny
  -- listen (headV <$$> VConst [3, 4, 5]) listeny
  -- listen (tailV <$$> VConst [3, 4, 5]) listeny
  listen (_aList <$$> vw) listeny
  listen (_anotherList <$$> vw) listeny
  listen (_zero <$$> vw) listeny
  -- listen (headV <$$> (_aList <$$> vw)) listeny
  -- listen (tailV <$$> (_aList <$$> vw)) listeny
  -- listen (consV <**> (headV <$$> (_aList <$$> vw))
  --               <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) listeny
  let mapped = mapVE incV (_aList <$$> vw)
  let aFold :: V Int
      aFold = foldrVE shiftNAddV (_zero <$$> vw) (_anotherList <$$> vw)
  listen mapped listeny
  listen aFold listeny
  invitedUsersV <--- VConst "" ["b", "heyo", "hippo"]
  modded <--- VConst "" ["c!", "deyo!", "lippo!"]
  mapped <--- VConst "" [302, 402, 502]
  aFold <--- VConst "" (456::Int)
  -- (headV <$$> (_aList <$$> vw)) <--- VConst 31
  -- (tailV <$$> (_aList <$$> vw)) <--- VConst [42, 52]
  -- Non-singular write
  -- (consV <**> (headV <$$> (_aList <$$> vw))
  --        <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) <--- VConst [310, 520]

extMain = do
  (a, history') <- tmiRun history action
  -- msp $ fyold (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ fyold' (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  msp $ "result " ++ show a
  -- msp history'
  -- runListeners history'
  -- msp $ case history' of History _ listeners -> length listeners
  -- msp $ map (+1) [3, 4, 5]
  -- msp $ mapE (+1) [3, 4, 5]
  -- msp $ mapE' (+1) [3, 4, 5]
  msp "ext hi"

-- $> :t invitedUsersV
