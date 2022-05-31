module Main where

import System.IO

import Util

main = do
  hSetBuffering stdout NoBuffering
  -- metaMain logApp
  -- logMain
  -- oldMain
  -- ledgerMain
  msp "hi"
