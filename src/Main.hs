module Main where

import System.IO

import GHCICleanup
import Old
import Util

main = do
  runCleanups
  hSetBuffering stdout NoBuffering
  oldMain
  msp "hi"
