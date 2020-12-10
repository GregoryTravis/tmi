{-# LANGUAGE
  ExistentialQuantification
, MultiParamTypeClasses
, RankNTypes
#-}

module TStream where

import Data.Dynamic
import Data.List (sortOn)
import Data.Typeable

import Internal
import Tmi

-- Most recent is first
data Stream a = Stream [a]

append :: a -> Stream a -> Stream a
append a (Stream as) = Stream (a : as)

type TStream a = Stream (Int, a)

tappend :: Int -> a -> TStream a -> TStream a
tappend t a (Stream as) = Stream ((t, a) : as)

data Wrapped = Wrapped Dynamic

wrapTStream :: (Eq a, Show a, Typeable a) => TStream a -> TStream Wrapped
wrapTStream (Stream tas) = Stream (map (onSnd (Wrapped . toDyn)) tas)
  where onSnd f (a, b) = (a, f b)
