module Main where

import System.IO

import Log

main = do
  hSetBuffering stdout NoBuffering
  logMain
