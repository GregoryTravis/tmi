{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( tmiMain ) where

import Util

class V v where
  type D v
  (.+) :: v -> D v -> v

-- data DInt = DIntAdd Int | DIntSub Int
-- instance V Int where
--   type D Int = DInt
--   x .+ (DIntAdd dx) = x + dx
--   x .+ (DIntSub dx) = x - dx

data DNum n = DNumAdd n | DNumSub n
instance Num n => V n where
  type D n = DNum n
  x .+ (DNumAdd dx) = x + dx
  x .+ (DNumSub dx) = x - dx

tmiMain = do
  msp $ 3 .+ (DNumAdd 12)
  msp "hihi"
