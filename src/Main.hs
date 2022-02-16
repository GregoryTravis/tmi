module Main where

import System.IO

import Ledger
import Log
import Meta
import Old
import Util

main = do
  hSetBuffering stdout NoBuffering
  -- metaMain logApp
  -- logMain
  -- oldMain
  ledgerMain
  msp "hi"
