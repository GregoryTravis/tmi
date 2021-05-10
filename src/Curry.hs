{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Control.Monad.Cont

import Util

-- data Write1 = forall a. Write1 a
data Write1 = forall a. Show a => Write1 (V a) a
deriving instance Show Write1
data Write = Write [Write1] deriving Show
instance Semigroup Write where
  Write ws <> Write ws' = Write $ ws ++ ws'
data Receiver a = Receiver (a -> Write)
instance Show (Receiver a) where
  show (Receiver _) = "Receiver"
-- data Receiver a = Receiver (V a)
data R a = R a (Receiver a)
  deriving Show
infix 1 <--
(<--) :: Receiver a -> a -> Write
Receiver r <-- x = r x
-- Receiver va <-- x = Write [Write1 va x]
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

data W = W { anInt :: Int, anotherInt :: Int }
  deriving Show
world :: W
world = W { anInt = 10, anotherInt = 20 }

_anInt :: V (R W -> R Int)
_anInt = VConst __anInt
  where __anInt (R w rw) = (R i ri)
          where i = anInt w
                ri = Receiver $ \newI ->
                    rw <-- w { anInt = newI }

_anotherInt :: V (R W -> R Int)
_anotherInt = VConst __anotherInt
  where __anotherInt (R w rw) = (R i ri)
          where i = anotherInt w
                ri = Receiver $ \newI ->
                    rw <-- w { anotherInt = newI }

data V a where
  VRoot :: V W
  VConst :: Show a => a -> V a
  VPartialApp :: Show a => V (R a -> rest) -> V a -> V rest
  VApp :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a) => V (R a) -> V a

-- more succinct
k :: Show a => a -> V a
k = VConst
root = VRoot
infixl 4 <**>
(<**>) :: Show a => V (R a -> rest) -> V a -> V rest
(<**>) = VPartialApp
infixl 4 <$$>
(<$$>) :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
(<$$>) = VApp

-- app2 :: V (R a -> R b -> R c) -> V a -> V (R b -> R c)
-- partialApp :: V (R a -> rest) -> V a -> V rest
-- partialApp = undefined

instance Show (a -> b) where
  show _ = "fn"

instance Show a => Show (V a) where
  show VRoot = "[root]"
  show (VConst a) = show a
  show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ (show vfb) ++ ")"
  show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ (show va) ++ ")"
  show (VSeal va) = "(seal " ++ (show va) ++ ")"

-- TODO maybe tf for this?
incV :: V (R Int -> R Int)
incV = VConst inc_hy

plus :: V (R Int -> R Int -> R Int)
plus = VConst plus_hy

vw :: V W
vw = VRoot

anIntV = _anInt <$$> vw
anotherIntV = _anotherInt <$$> vw
inced = incV <$$> anIntV
plusPartialV = plus <**> anIntV
plusPartialV' = plusPartialV <**> anotherIntV
sumV = plusPartialV <$$> anotherIntV
sumV' = VSeal plusPartialV'
sumV'' = plus <**> anIntV <$$> anotherIntV

r :: W -> V a -> a
r w VRoot = w
r _ (VConst x) = x
-- TODO not crazy about constructing receivers here
r w (VApp vf va) = b
  where f = r w vf
        a = r w va
        -- rb = R b (Receiver $ \b' -> Write [Write1 b'])
        ra = R a (Receiver $ \a' -> Write [Write1 va a'])
        rb = f ra
        b = case rb of R b _ -> b
r w (VSeal vra) = a
  where ra = r w vra
        a = case ra of R a _ -> a
r w (VPartialApp vf va) = paf
  where f = r w vf
        a = r w va
        ra = R a (Receiver $ \a' -> Write [Write1 va a'])
        paf = f ra

wr :: W -> V a -> a -> Write
wr w VRoot _ = undefined "Can't write to root"
wr w (VConst _) _ = undefined "Can't write to a const"
wr w (VApp vfbfa vb) b = wr w (VSeal (VPartialApp vfbfa vb)) b
  --where -- write = Write [Write1 vb b']
  --      write = reca a
  --      rbra = r w vfbfa
  --      -- ra = rbra rb
  --      R _ (Receiver reca) = rbra rb
  --      rb = R b (Receiver $ \b' -> Write [Write1 vb b'])
  --      b = r w vb
  --      --b' = undefined
-- Good gravy why is this not needed?
wr w (VPartialApp vf va) _ = error "Why is this error not happening"
wr w (VSeal vra) a = write
  where write = reca a
        R _ (Receiver reca) = ra
        ra = r w vra

curryMain = do
  -- msp $ r world vw
  -- msp $ r world anIntV
  -- msp $ r world inced
  msp $ r world sumV
  msp $ r world sumV'
  -- msp $ wr world anIntV 100
  -- msp $ wr world anIntV 100
  -- msp $ wr world sumV 100
  -- msp $ wr world sumV' 100
  msp $ wr world sumV'' 100
  -- msp $ wr world (VApp incV sumV) 201
  -- msp $ wr world (VApp incV sumV') 201
  msp $ wr world (VApp incV sumV'') 201
  msp "curry hi"

-- $> :module +*Curry
--
-- $> :t sumV''
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
