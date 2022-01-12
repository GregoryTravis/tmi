{-# Language RecordWildCards #-}

module InternalCallRunner
( mkInternalCallRunner
, InternalCallRunner
, icrRead
, icrDone
, icrInFlight
, icrRun
) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (when)
import qualified Data.Set as S

import Core
import Util

data InternalCallRunner w = InternalCallRunner
  { eventChan :: Chan (Event w)
  , inFlightCount :: MVar Int
  , started :: S.Set Int }

updateInFlightCount :: InternalCallRunner w -> (Int -> Int) -> IO ()
updateInFlightCount icr f = do
  -- c <- readMVar (inFlightCount icr)
  -- msp $ "update before " ++ show c
  modifyMVar_ (inFlightCount icr) (return . f)
  -- c' <- readMVar (inFlightCount icr)
  -- msp $ "update after " ++ show c'

mkInternalCallRunner :: IO (InternalCallRunner w)
mkInternalCallRunner = do
  eventChan <- newChan
  inFlightCount <- newMVar 0
  let started = S.empty
  let icr = InternalCallRunner {..}
  return icr

-- Read an event from the event change. If it's internal decrease the in-flight count
icrRead :: InternalCallRunner w -> IO (Event w)
icrRead icr = do
  e <- readChan (eventChan icr)
  -- msp $ "count dec 1"
  updateInFlightCount icr (subtract 1)
  return e

-- True if there are no calls (internal) in flight
icrDone :: InternalCallRunner w -> IO Bool
icrDone icr = do
  b <- readMVar (inFlightCount icr)
  return $ b == 0

icrInFlight :: InternalCallRunner w -> IO Int
icrInFlight icr = do
  readMVar (inFlightCount icr)

-- Read a ce, pick short calls to run, start them, repeat.
icrRun :: InternalCallRunner w -> [Call w] -> [Event w] -> IO (InternalCallRunner w)
icrRun icr calls events = do
  let (callsAndIndices, started') = needToRun (started icr) calls events
  runCalls icr callsAndIndices
  return $ icr { started = started' }

runCalls :: InternalCallRunner w -> [(Int, Call w)] -> IO ()
runCalls icr callsAndIndices = do
  -- when (not (null callsAndIndices)) $ msp ("runCalls", map snd callsAndIndices)
  runList_ ios
  -- msp $ "count inc " ++ show numInternalCalls
  -- msp callsAndIndices
  updateInFlightCount icr (+ numInternalCalls)
  where ios = map (\(i, call) -> do wrapFork $ wrapAction (eventChan icr) i call) callsAndIndices
        numInternalCalls = length $ filter isInternal (map snd callsAndIndices)
        isInternal (InternalCall _ _ _) = True
        isInternal _ = False

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
