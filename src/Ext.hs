{-# LANGUAGE ExistentialQuantification, NumericUnderscores #-}

module Ext
( extMain
) where

import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import Data.Time.Clock.System (getSystemTime, SystemTime)

import Tmi
import Util

{-
-- req/resp must be nice (storable), therefore they must be instances?

world :: W
world = W
  { reqs = emptyStream
  , resps = emptyStream }

sleepAndReturn :: Show a => Double -> a -> IO a
sleepAndReturn delayS x = do
  msp $ "start sleep " ++ (show x)
  let delay = floor $ 1_000_000 * delayS
  return x

-- generally: a -> IO b

data Req a = Req Int a deriving (Read, Show)
data Resp a = Resp a deriving (Read, Show)

class Requestable a where
  toRequest :: a -> Request

data Request = forall a. Request
  { toIO :: IO a
  , toConsequence :: a -> TMI W () }

type Timestamp = Int64

data Stream a = Stream [(Timestamp, a)] deriving (Read, Show)
emptyStream :: Stream a
emptyStream = Stream []
isEmpty :: Stream a -> Bool
isEmpty (Stream []) = True
isEmpty _ = False

appendStream :: Stream a -> Timestamp -> a -> Stream a
appendStream st@(Stream evs) t ev = assertM "out of order" ok appended
  where ok | isEmpty st = True
           | case evs of ((latest, _):_) -> latest < t = True
           | otherwise = False
        appended = Stream ((t, ev):evs)

data W = W { reqs :: Stream Requestable
           , resps :: Stream (Resp String) }
  deriving (Read, Show)

history :: History W
history = mkHistory world
-}

extMain = do
  msp "ext hi"
