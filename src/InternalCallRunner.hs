module InternalCallRunner
( mkInternalCallRunner
) where

import Control.Concurrent
import Control.Monad (when)
import qualified Data.Set as S

import Monad
import Util

mkInternalCallRunner :: Chan (Event w) -> IO (Chan ([Call w], [Event w]))
mkInternalCallRunner eventChan = do
  ceChan <- newChan
  forkIO (startLoop eventChan ceChan)
  return ceChan

startLoop :: Chan (Event w) -> Chan ([Call w], [Event w]) -> IO ()
startLoop = loop S.empty

-- Read a ce, pick short calls to run, start them, repeat.
loop :: S.Set Int -> Chan (Event w) -> Chan ([Call w], [Event w]) -> IO ()
loop started eventChan ceChan = do
  -- msp "ICR wait"
  (calls, events) <- readChan ceChan
  -- msp ("ICR done waiting", length calls, events)
  let (callsAndIndices, started') = needToRun started calls events
  runCalls eventChan callsAndIndices
  loop started' eventChan ceChan

runCalls :: Chan (Event w) -> [(Int, Call w)] -> IO ()
runCalls chan callsAndIndices = do
  when (not (null callsAndIndices)) $ msp ("runCalls", map snd callsAndIndices)
  runList_ ios
  where ios = map (\(i, call) -> wrapFork $ wrapAction chan i call) callsAndIndices

wrapFork :: IO () -> IO ()
wrapFork io = do
  -- msp "forking"
  forkIO io
  return ()

-- From the provided calls list, remove:
-- - Calls that already have retvals in the list
-- - Calls that are in 'started'
-- Also updated 'started':
-- - Add the calls we are about to return for execution
-- - Remove all the calls that have retvals in the list
needToRun :: S.Set Int -> [Call w] -> [Event w] -> ([(Int, Call w)], S.Set Int)
needToRun started calls events =
  let -- Only retval events
      retvals = filter (\e -> case e of Retval _ _ -> True; _ -> False) events
      -- Their indices
      retvalIndices = map (\e -> case e of Retval i _ -> i) retvals
      -- Don't do ones we've started or ones in retval
      dontDoIndices = S.fromList retvalIndices `S.union` started
      -- Do everything else
      doCallsAndIndices = filter (\(i, call) -> not (i `S.member` dontDoIndices))
                                 (zip [0..] calls)
      doIndices = map fst doCallsAndIndices
      -- Add the ones we're starting; remove the ones in retvals
      started' = (started `S.union` S.fromList doIndices) `S.difference` S.fromList retvalIndices
      info = [ ("events", show events)
             , ("retvals", show retvals)
             , ("num calls", show $ length calls)
             , ("calls", show calls)
             , ("retvalIndices", show retvalIndices)
             , ("dontDoIndices", show dontDoIndices)
             , ("doIndices", show doIndices)
             , ("started", show started)
             , ("started'", show started')]
  in noeesp info (doCallsAndIndices, started')
