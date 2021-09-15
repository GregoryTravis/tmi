{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Trace where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Data.Maybe (isNothing)

import ExecId
import Ext
import Lens
import Util
import W

data Consequence = Consequence Call Resp

clearOutStaleInitiations :: ExecId -> [Call] -> [Call]
clearOutStaleInitiations execId = map (clearOutStaleInitiation execId)
  where clearOutStaleInitiation currentExecId
                                c@Call { initiation = Just (Initiation execId) } 
          | execId == currentExecId = c
          | otherwise = c { initiation = Nothing }
        clearOutStaleInitiation _ c = c

requestsThatNeedInitiation :: [Call] -> [Call]
requestsThatNeedInitiation = filter needInitiation
  where needInitiation call = isNothing $ initiation call

refreshRpcs :: Chan Consequence -> ExecId -> Rpc c -> IO (Rpc c)
refreshRpcs consequencesChan execId rpc@Rpc {..}  = do
  -- Clear out stale initiations
  let calls' = clearOutStaleInitiations execId calls
      -- Get requests that need initiation
      needInit :: [Call]
      needInit = requestsThatNeedInitiation calls'
      -- Initiate them
      ios = map toIOV calls'
        where toIOV :: Call -> IO ()
              toIOV call@Call { req } = do
                resp <- toExt req
                writeChan consequencesChan (Consequence call resp)
                return ()
      -- New initiation to write
      init = Initiation execId
      -- ids of calls we are about to initiate
      uids = map callUniqueId needInit
      -- mark the calls with the initiation
      calls'' = map setInit calls'
        where setInit c@Call { callUniqueId = uid }
                | uid `elem` uids = c { initiation = Just init }
                | otherwise = c
  launchIOs ios
  return rpc { calls = calls'' }

commitResponse :: Consequence -> Rpc c -> Rpc c
commitResponse (Consequence Call { callUniqueId = lookingForCallUniqueId } theResp) rpc =
  rpc { calls = findAndReplace (calls rpc) }
  where findAndReplace [] = error ("Could not find call " ++ show callUniqueId)
        findAndReplace (call:calls)
          | callUniqueId call == lookingForCallUniqueId && isNothing (resp call) =
              (call { resp = Just theResp }) : calls
          | callUniqueId call == lookingForCallUniqueId =
              error ("Call already has a response: " ++ show call)
          | otherwise = call : findAndReplace calls

launchIOs :: [IO ()] -> IO ()
launchIOs = mapM_ forkIO

_calls = mkFielder "_calls" calls $ \w a -> w { calls = a }
