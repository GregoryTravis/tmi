{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Evaluator
import Hash
import Tmi
import Util

data W = W { anInt :: Int }
  deriving (Eq, Show)

anIntF :: V W -> V Int
anIntF = hoist_1_1 $ F {..}
  where ffor = anInt
        frev w i = w { anInt = i }
        name = "anIntF"

world :: W
world = W { anInt = 100 }
aW = konstV world

-- 'plus' lens: adds forwards; backwards, splits value into two roughly equal halves
-- The 'F' suffix isn't right, these aren't Fs
plusF :: (Typeable a, Integral a) => V a -> V a -> V a
plusF = hoist_2_1 $ F2 {..}
  where ffor2 = (+)
        frev2 _ _ x = (x `div` 2, x - (x `div` 2))
        name2 = "plusF"

-- TODO If I annotate this, I get weird typeclass errors
-- aV :: (Show a, Typeable a, Integral a) => V a
--aV = plusF (konstV (40::Int)) (konstV 60)
aV = anIntF aW

incF :: (Typeable a, Integral a) => V a -> V a
incF = hoist_1_1 $ F {..}
  -- TODO: why can't these be (+1) and (subtract 1) -- it says it can't deduce Num (from Integral??)
  where ffor x = x + 1
        frev _ x = x - 1
        name = "incF"

anotherV = incF aV

splitF :: (Typeable a, Integral a) => V a -> (V a, V a)
splitF = hoist_1_2 $ F_1_2 {..}
  where ffor_1_2 x = (x', x'')
          where x' = x `div` 2
                x'' = x - x'
        frev_1_2 _ (x, x') = x + x'
        name_1_2 = "splitF"

-- awrite :: string
-- awrite = show $ ((case writev av (260::int) of [dyx, dyy] -> (undy dyx, undy dyy)) :: (int, int))

-- anotherwrite :: string
-- anotherwrite = show $ ((case writev anotherv (281::int) of [dyx] -> undy dyx) :: int)

-- Net
--
-- aV --> anotherV --> leftV, rightV

main = do
  msp ("aW", aW)
  msp ("aV", aV)
  msp ("aW N", vN aW)
  let (leftV, rightV) = splitF anotherV
  let evaluator = Simple [dyv leftV, dyv rightV]
  aVValue <- readV evaluator aV
  anotherVValue <- readV evaluator anotherV
  msp (aVValue, anotherVValue)
  leftValue <- readV evaluator leftV
  rightValue <- readV evaluator rightV
  msp (leftValue, rightValue)
  let write = Write (dyv leftV) (dy (100::Int))
  applyWrites evaluator [write]
  let write' = Write (dyv rightV) (dy (101::Int))
  applyWrites evaluator [write']
  msp "hi"
