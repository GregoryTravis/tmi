{-# LANGUAGE TypeFamilies #-}

module Tmi
( tmiMain ) where

import Util

class V v where
  type D v
  (.+) :: v -> D v -> v

data DInt = DIntAdd Int | DIntSub Int
instance V Int where
  type D Int = DInt
  x .+ (DIntAdd dx) = x + dx
  x .+ (DIntSub dx) = x - dx

tmiMain = do
  msp $ (3::Int) .+ (DIntAdd (12::Int))
  msp "hihi"
