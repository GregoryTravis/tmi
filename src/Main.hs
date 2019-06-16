{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import Util

{-
- External derivative of regular function?
- Lam EDSL
- Take the derivative of +
-}
class Deltable a da | a -> da where
  (.+) :: a -> da -> a
  (.-) :: a -> a -> da

instance Deltable Int Int where
  x .+ dx = x + dx
  x .- y = y - x

{-
--instance (Num a, Num b) => Deltable a b where
instance Num a => Deltable a a where
  x .+ dx = x + dx
  x .- y = y - x
-}

{-
data ConsDelta a = HeadDelta a | TailDelta [a] | ConsDeltas [ConsDelta a]

instance Deltable (:) ConsDelta
  (x:xs) .+ (HeadDelta nx) = nx:xs
  (x:xs) .+ (TailDelta nxs) = x:nxs
  (x:xs) .- (y:ys) | x == y = 
-}

data Prod a b = Prod a b
  deriving Show
--data Sum a b = Either a b

data ProdDelta a b = FirstDelta a | SecondDelta b | ProdDeltas [ProdDelta a b]
  deriving Show

instance (Eq a, Eq b) => Deltable (Prod a b) (ProdDelta a b) where
  (Prod a b) .+ (FirstDelta na) = Prod na b
  (Prod a b) .+ (SecondDelta nb) = Prod a nb
  p .+ (ProdDeltas (d:ds)) = (p .+ d) .+ (ProdDeltas ds)
  (Prod a0 b0) .- (Prod a1 b1)
    | a0 == a1 && b0 == b1 = ProdDeltas []
    | a0 == a1 = SecondDelta b1
    | b0 == b1 = FirstDelta a1
    | otherwise = ProdDeltas [FirstDelta a1, SecondDelta b1]

data Sum a b = SumLeft a | SumRight b
  deriving Show
data SumDelta a b = SumDelta (Sum a b)
  deriving Show

instance (Eq a, Eq b) => Deltable (Sum a b) (SumDelta a b) where
  _ .+ (SumDelta sd) = sd
  _ .- b = SumDelta b

-- Insert is interstice position; delete is array position
data ListDelta a = Insert Int a | Delete Int
  deriving Show

instance Deltable [a] (ListDelta a) where
  as .+ (Insert i na) | i < 0 || i > length as = undefined
                      | otherwise = take i as ++ [na] ++ drop i as
  as .+ (Delete i) | i < 0 || i >= length as = undefined
                   | otherwise = take i as ++ drop (i+1) as
  -- Not plausible
  as .- _ = undefined

-- Different instances for the derivative of map
--map_d :: Deltable a da => (a -> b) -> [a] -> da -> [b]
map_d :: (a -> b) -> [a] -> ListDelta a -> [b]
map_d f as (Insert i na) = (map f as) .+ (Insert i (f na))
map_d f as (Delete i) = map f (as .+ Delete i)

main = do
  lists
  intsSumsProds
  maps

maps = do
  let as = [1, 2, 3, 4, 5]
  msp $ map_d (2*) as (Insert 2 21)
  msp $ map_d (2*) as (Delete 3)
  msp "hi"

lists = do
  let a = [10, 20, 30, 40, 50]
      b = 21
      c = a .+ (Insert 2 b)
      d' = 3
      d = Delete d'
      e = a .+ d
  msp c
  msp e
  msp $ [10, 20, 30, 40, 50] .+ Insert 2 21
  msp $ [10, 20, 30, 40, 50] .+ Delete 3

intsSumsProds = do
  --lists
  let a = 9 :: Int
      b = 11
      c = (a .- b)
  msp c
  --msp $ 9 .- 11
  let aa = Prod "asdf" "zxcv"
      bb2 = Prod "asdf" "vbnm"
      bb = Prod "qwer" "vbnm"
      cc = (aa .- bb)
      cc2 = (aa .- bb2)
      dd2 = aa .+ cc2
  msp cc
  msp cc2
  msp dd2
  msp $ (Prod "asdf" "zxcv") .- (Prod "asdf" "vbnm")
  let aaa = SumLeft 12 :: Show a => Sum Int a
      bbb = SumRight "foo"
      ccc = (aaa .- bbb)
      ddd = aaa .+ ccc
  msp aaa
  msp ccc
  msp ddd
