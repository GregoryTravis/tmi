{-# LANGUAGE RecordWildCards #-}

module ExtRunner
( ExtRunner
, mkExtRunner
, run ) where

import Control.Concurrent
import Control.Concurrent.Chan

import Util

data ExtRunner a = ExtRunner (Chan a)

mkExtRunner :: IO (ExtRunner a)
mkExtRunner = do
  chan <- newChan
  return $ ExtRunner chan

run :: ExtRunner a -> IO a -> IO ()
run (ExtRunner chan) ioa = do
  forkIO $ do
    a <- ioa
    msp "RUNNING IOTS 2"
    writeChan chan a
  return ()
