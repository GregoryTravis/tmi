{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Control.Monad.Cont

import Util

data Write1 = forall a. Write1 a
-- data Write1 = forall a. Write1 (V a) a
data Write = Write [Write1]
instance Semigroup Write where
  Write ws <> Write ws' = Write $ ws ++ ws'
data Receiver a = Receiver (a -> Write)
-- data Receiver a = Receiver (V a)
data R a = R a (Receiver a)
infix 1 <--
(<--) :: Receiver a -> a -> Write
rx <-- x = Write [Write1 x]
-- Receiver va <-- x = Write [Write1 va x]

inc_hy :: R Int -> R Int
inc_hy (R x rx) = R x' rx'
  where x' = x + 1
        rx' = Receiver $ \x ->
          rx <-- (x - 1)
inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: R Int -> Int -> Write
inc_rev (R x rx) newX =
  rx <-- newX - 1
inc_hy' :: R Int -> R Int
inc_hy' = hybrid1 inc_for inc_rev

hybrid1 :: (a -> b) -> (R a -> b -> Write) -> (R a -> R b)
hybrid1 f r ra@(R x rx) = R x' rx'
  where x' = f x
        rx' = Receiver $ \x -> r ra x

plus_hy :: R Int -> R Int -> R Int
plus_hy (R x rx) (R y ry) = R z rz
  where z = x + y
        rz = Receiver $ \newZ ->
          let x' = newZ `div` 2
              y' = newZ - x'
           in (rx <-- x') <>
              (ry <-- y')
plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: R Int -> R Int -> Int -> Write
plus_rev (R x rx) (R y ry) newZ =
  (rx <-- x') <>
  (ry <-- y')
  where x' = newZ `div` 2
        y' = newZ - x'
plus_hy' :: R Int -> R Int -> R Int
plus_hy' = hybrid2 plus_for plus_rev

hybrid2 :: (a -> b -> c) -> (R a -> R b -> c -> Write) -> (R a -> R b -> R c)
hybrid2 f r ra@(R x rx) rb@(R y ry) = R z rz
  where z = f x y
        rz = Receiver $ \x -> r ra rb x

data W = W { anInt :: Int }
  deriving Show
world :: W
world = W { anInt = 10 }

_anInt :: R W -> R Int
_anInt (R w rw) = (R i ri)
  where i = anInt w
        ri = Receiver $ \newI ->
            rw <-- w { anInt = newI }

data V a where
  VRoot :: V W
  VConst :: a -> V a
  VApp :: (V (R b -> R a)) -> V b -> V a

-- TODO maybe tf for this?
incV :: V (R Int -> R Int)
incV = VConst inc_hy

vw :: V W
vw = VRoot

anIntV = VApp (VConst _anInt) vw
inced = VApp incV anIntV

r :: W -> V a -> a
r w VRoot = w
r _ (VConst x) = x
-- TODO not crazy about constructing receivers here
r w (VApp vf vx) = a
  where rbra = r w vf
        b = r w vx
        rb = R b (Receiver $ \b' -> Write [Write1 b'])
        ra = rbra rb
        a = case ra of R a _ -> a

write :: W -> V a -> a -> Write
write w VRoot _ = undefined "Can't write to root"
write w (VConst _) _ = undefined "Can't write to a const"
write w (VApp vf vx) _ = undefined

curryMain = do
  msp $ r world vw
  msp $ r world anIntV
  msp $ r world inced
  msp "curry hi"

-- $> :module +*Curry
--
-- $> :t anIntV
--
-- $> :t inced
--
-- $> curryMain

--data V a where
--  VRoot :: V (B W)
--  VConst :: F f r -> V (F f r)
--  --VConst :: F f r -> V (F f r)
--  VConst :: a  -> V a
--  VApp :: V (F (b -> a) (R b -> c)) -> V (B b) -> V (F a c)

-- -- covar :: F (b -> a) -> F a -> F b
-- -- covar :: F (c -> b -> a) -> F (b -> a) -> F c

-- data F x a = F (x -> a)
-- data R a y = R (a -> y)
-- data V x a y = V (F x a) (R a y)

-- lah :: V x (a -> b) y -> V x a y -> V x b y
-- lah (V (F tof) (R fromf)) (V (F toa) (R froma)) = V (F toy) (R fromb)
--   where toy x = (tof x) (toa x)
--         -- fromf :: ((a -> b) -> y)
--         -- froma :: (a -> y)
--         -- fromb :: (b -> y)
--         -- whaa :: ((a -> b) -> y) -> (a -> y) -> (b -> y)
--         -- ???? :: ((b -> a) -> y) -> (a -> y) -> (b -> y)
--         fromb = whaa fromf froma
--         whaa = undefined
