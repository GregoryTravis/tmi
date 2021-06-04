module Tmi
( R(..)
, Write(..)
, Write1(..)
, emptyWrite
, V(..)
, Receiver(..)
, hybrid1
, hybrid2
, hybrid3
, (<--)
, (<**>)
, (<$$>)
, History(..)
, TmiState(..)
, mkHistory
, getRoot
, TMI
, tmiRun
, persistentTmiRun
, writeHistory -- TODO remove, only for setting up
, (<---)
, listen
-- TODO remove after moving listeners out of history
, runListeners
) where

import Data.Dynamic

import Curry
