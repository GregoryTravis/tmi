module Main where

import Control.Monad.State hiding (lift)
import Data.Dynamic

import Ext
import Narrative
import Tmi
import Util

main = narrativeMain
