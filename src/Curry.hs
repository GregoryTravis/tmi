{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards #-}

module Curry (curryMain) where

import Util

data W = W

data Write = Write

data R a = R

-- TODO other arities
data F a b = F a b

data V a where
  VRoot :: V W
  VConst :: a -> V a
  VApp :: App a -> V a

r :: V a -> a
r VRoot = W
r (VConst x) = x
r (VApp app) = runAppForwards app

-- TODO inline this into V?
data App a = forall b. App (V (F (b -> a) ((R b, b) -> a -> Writes))) (V b)

app :: V (F (b -> a) ((R b, b) -> a -> Writes)) -> V b -> V a
app vf vb = VApp $ App vf vb

app' :: V (F (b -> c) ((R b, b) -> d)) -> V b -> V (F c d)
app' = undefined

data App'' c d = forall b. App'' (V (F (b -> c) ((R b, b) -> d))) (V b)

runAppForwards :: App a -> a
runAppForwards (App vf vx) = f x
  where f = case r vf of (F f _) -> f
        x = r vx

type Writes = [Write]
infix 8 <--
(<--) :: Show a => R a -> a -> [Write]
rx <-- x = [Write]

inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: (R Int, Int) -> Int -> Writes
inc_rev (rx, _) x =
  rx <-- x'
  where x' = x - 1

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: (R Int, Int) -> (R Int, Int) -> Int -> Writes
plus_rev (rx, x) (ry, y) nZ =
  rx <-- x' <>
  ry <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

inc = VConst (F inc_for inc_rev)
plus = VConst (F plus_for plus_rev)
three = VConst 3
threeInced = app inc three
threeInced2x = app inc threeInced
four = VConst (4::Int)
seven = app' (app' plus three) four

--runFor :: App a

curryMain = do
  msp $ r three
  msp $ r threeInced
  msp $ r threeInced2x
  msp $ case r seven of Curry.F x _ -> x
  msp "curry hi"
