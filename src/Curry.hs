{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Control.Applicative

import Util

data Write = Write

instance Semigroup Write where
  Write <> Write = Write

data R a = R (a -> Write)
data V a = V a (R a)

inc :: V Int -> V Int
inc (V x (R rx)) = V x' (R rx')
  where x' = x + 1
        rx' x' = rx (x - 1)

plus :: V Int -> V Int -> V Int
plus (V x (R rx)) (V y (R ry)) = V z (R rz)
  where z = x + y
        rz z' = rx x' <> ry y'
          where x' = z' `div` 2
                y' = z' - x'

curryMain = do
  msp "curry hi"
