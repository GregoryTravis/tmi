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

-- TODO If I annotate this, I get weird typeclass errors
-- aV :: (Show a, Typeable a, Integral a) => V a
aV = plusF (konstV 40) (konstV 60)

aWrite :: String
aWrite = show $ ((case w aV (260::Int) of [dyx, dyy] -> (undy dyx, undy dyy)) :: (Int, Int))

main = do
  msp (r aV, aWrite)
  msp "hi"
