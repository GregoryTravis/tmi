{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, TypeApplications, TypeFamilies #-}

module Curry (curryMain) where

import Util

data Write = Write
type Writes = [Write]
infix 8 <--
(<--) :: Show a => R a -> a -> [Write]
rx <-- x = [Write]

-- e.g.
-- for: a -> b -> c
-- rev: a -> R a -> b -> R b -> c -> R c

data R a = R (a -> Writes)

-- TODO maybe this is just V
data F a = F a (Rev a)

app :: F (a -> b) -> F a -> F b
-- app :: F (a -> b) (a -> R a -> b -> R b)
--     -> F a (a -> R a)
--     -> F b (b -> R b)
app (F f_for f_rev) (F a_for a_rev) = undefined

type family Rev a where
  Rev (a -> b) = (a -> R a -> Rev b)
  Rev a = a -> R a

inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: Int -> R Int -> Int -> R Int
inc_rev _x rx x = R $ \_ ->
  rx <-- x'
  where x' = x - 1
inc :: F (Int -> Int)
inc = F inc_for inc_rev

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: Int -> R Int -> Int -> R Int -> Int -> R Int
plus_rev x rx y ry nZ = R $ \_ ->
  rx <-- x' <>
  ry <-- y'
  where x' = nZ `div` 2
        y' = nZ - x'

plus :: F (Int -> Int -> Int)
plus = F plus_for plus_rev

curryMain = do
  msp "curry hi"
