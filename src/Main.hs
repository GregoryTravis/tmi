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
aV = plusF (konstV 40) (konstV 60)

incF :: (Typeable a, Integral a) => V a -> V a
incF = hoist_1_1 $ F {..}
  -- TODO: why can't these be (+1) and (subtract 1) -- it says it can't deduce Num (from Integral??)
  where ffor x = x + 1
        frev _ x = x - 1
        name = "incF"

anotherV = incF aV

aWrite :: String
aWrite = show $ ((case w aV (260::Int) of [dyx, dyy] -> (undy dyx, undy dyy)) :: (Int, Int))

anotherWrite :: String
anotherWrite = show $ ((case w anotherV (281::Int) of [dyx] -> undy dyx) :: Int)

main = do
  msp (r aV, r anotherV, aWrite, anotherWrite)
  msp "hi"
