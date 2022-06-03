module Main where

import System.IO

import Old
import Util

main = do
  hSetBuffering stdout NoBuffering
  oldMain
  msp "hi"
