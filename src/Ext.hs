{-# LANGUAGE ExistentialQuantification, NumericUnderscores #-}

module Ext
( extMain
) where

import Control.Concurrent (threadDelay)
import Data.Int (Int64)
import Data.Time.Clock.System (getSystemTime, SystemTime)

import Tmi
import Util

extMain = do
  msp "ext hi"
