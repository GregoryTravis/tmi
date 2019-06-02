{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Util

{-
- External derivative of regular function?
- Lam EDSL
- Take the derivative of +
-}
class Deltable a da where
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

main = do
  let a = 9 :: Int
      b = 11 :: Int
      c = (a .- b) :: Int
  msp c
  let aa = Prod "asdf" "zxcv"
      bb2 = Prod "asdf" "vbnm"
      bb = Prod "qwer" "vbnm"
      cc = (aa .- bb) :: ProdDelta String String
      cc2 = (aa .- bb2) :: ProdDelta String String
      dd2 = aa .+ cc2
  msp cc
  msp cc2
  msp dd2
  let aaa = SumLeft 12 :: Sum Int String
      bbb = SumRight "foo" :: Sum Int String
      ccc = (aaa .- bbb) :: SumDelta Int String
      ddd = aaa .+ ccc
  msp aaa
  msp ccc
  msp ddd
  msp "hi"
