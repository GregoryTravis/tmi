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

-- 'plus' lens: adds forwards; backwards, splits value into two roughly equal halves
plusF :: (Typeable a, Integral a) => V a -> V a -> V a
plusF = hoist_2_1 $ F2 {..}
  where ffor2 = (+)
        frev2 _ _ x = (x `div` 2, x - (x `div` 2))
        name2 = "plusF"

-- TODO If I annotate this, I get weird typeclass errors
-- aV :: (Show a, Typeable a, Integral a) => V a
aV = plusF (konstV (40::Int)) (konstV 60)

incF :: (Typeable a, Integral a) => V a -> V a
incF = hoist_1_1 $ F {..}
  -- TODO: why can't these be (+1) and (subtract 1) -- it says it can't deduce Num (from Integral??)
  where ffor x = x + 1
        frev _ x = x - 1
        name = "incF"

anotherV = incF aV

-- awrite :: string
-- awrite = show $ ((case writev av (260::int) of [dyx, dyy] -> (undy dyx, undy dyy)) :: (int, int))

-- anotherwrite :: string
-- anotherwrite = show $ ((case writev anotherv (281::int) of [dyx] -> undy dyx) :: int)

main = do
  let evaluator = Simple
  aVValue <- readV evaluator aV
  anotherVValue <- readV evaluator anotherV
  msp (aVValue, anotherVValue)
  msp "hi"
