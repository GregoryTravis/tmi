{-# LANGUAGE TypeFamilies #-}

module Tmi
( tmiMain ) where

import Util

class Delta d where
  type V d
  (.+) :: V d -> d -> V d

data W = W { anInt :: Int }
  deriving Show

_anInt :: W -> Int -> W
_anInt w i = w { anInt = i }

w :: W
w = W { anInt = 10 }

tmiMain = do
  msp w
  msp $ _anInt w 11
  msp "hihi"
