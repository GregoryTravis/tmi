module Main where

import Control.Monad.State hiding (lift)
import Data.Dynamic

import Ext
import Monies
import Narrative
import Tmi
import Util

-- TODO do not swat me
instance Show (a -> b) where
  show _ = "fn"

main = extMain
