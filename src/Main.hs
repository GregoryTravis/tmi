module Main where

import System.IO

import Log
import Meta
import Util

main = do
  hSetBuffering stdout NoBuffering
  metaMain logApp
  msp "hi"
