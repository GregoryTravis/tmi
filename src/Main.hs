module Main where

import System.IO

import Log
import Util

main = do
  hSetBuffering stdout NoBuffering
  logMain
  msp "hi"
