module Main where

import System.IO

import Log
import Meta
import Old
import Util

main = do
  hSetBuffering stdout NoBuffering
  -- metaMain logApp
  -- logMain
  oldMain
  msp "hi"
