module Tmi
( W(..)
, R(..)
, Write
, V(..)
, Receiver(..)
, hybrid1
, hybrid2
, (<--)
, (<**>)
, (<$$>)
, History(..)
, SimpleHistory
, TMI
, tmiRun
, persistentTmiRun
, writeHistory -- TODO remove, only for setting up
, (<---)
, listen
) where

import Data.Dynamic

import Curry
