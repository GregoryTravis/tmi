{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Delta
import Exp
import Tmi
import Util

class Delta a d where
  (.+) :: a -> d -> a
  (.-) :: a -> a -> d

data ListDelta a = Insert Int a | Delete Int
deriving instance Show a => Show (ListDelta a)

instance Delta [a] (ListDelta a) where
  xs .+ (Insert i x) = (take i xs) ++ [x] ++ (drop i xs)
  (.-) = undefined  -- slow

voo = [1, 2, 3] :: [Int]
vep = Insert 2 4 :: ListDelta Int

main = do
  msp $ voo .+ vep
  msp $ ["a", "b", "c"] .+ Insert 2 "d"
  msp "hi"
  -- deltaDemo
  -- expDemo
  -- oldWebDemo
  -- bankProcess

{-
SO question

{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

class Foo a b where
  bar :: a -> b -> a

data Baz a = Baz a
instance Foo Int (Baz a) where
  bar i (Baz _) = i

standaloneBar :: a -> b -> a
standaloneBar x _ = x

main = do
  --putStrLn $ show $ bar 3 (Baz 4)          -- Can't unify
  putStrLn $ show $ standaloneBar 3 (Baz 4)  -- Works fine
  putStrLn $ show $ bar (3::Int) (Baz 4)     -- Works fine
  putStrLn $ show $ ((bar 3 (Baz 4)) :: Int) -- Works fine
-}
