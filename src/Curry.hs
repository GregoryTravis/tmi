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

-- r :: V a -> a
-- r VRoot = W

-- TODO inline this into V?
data App a = forall b. App (V (F (b -> a) ((R b, b) -> a -> Writes))) (V b)

app :: V (F (b -> a) ((R b, b) -> a -> Writes)) -> V b -> V a
app vf vb = VApp $ App vf vb

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

inc = VConst (F inc_for inc_rev)
three = VConst 3
threeInced = app inc three

--runFor :: App a

curryMain = do
  msp "curry hi"
