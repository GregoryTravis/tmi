module Main where

import System.IO

import GHCICleanup
import Old
import Util
import VPieces

main = do
  runCleanups
  hSetBuffering stdout NoBuffering
  vPiecesMain
  -- oldMain
  msp "hi"
