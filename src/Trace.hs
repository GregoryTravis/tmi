{-# LANGUAGE NumericUnderscores, RecordWildCards #-}

module Trace where

import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe (isNothing)

import ExecId
import Ext
import Lens
import Util

initRpc :: Rpc
initRpc = Rpc [] toExt -- toTmi
  where toExt r@(Req secs s) = io
          where io = do
                  threadDelay $ floor $ secs * 1_000_000
                  msp $ "Running ext: " ++ show r
                  return $ Resp $ s ++ "!"
        -- toTmi (Resp s) = do
        --   liftIO $ msp $ "Consequence: " ++ s

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

refreshRpcs :: ExecId -> Rpc -> IO Rpc
refreshRpcs execId rpc@Rpc {..}  = do
  -- Clear out stale initiations
  let calls' = clearOutStaleInitiations execId calls
      -- Get requests that need initiation
      needInit :: [Call]
      needInit = requestsThatNeedInitiation calls'
      -- Initiate them
      ios = map (toIOV . req) calls'
        where toIOV :: Req -> IO ()
              toIOV req = do
                toExt req
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
  return rpc
    -- where runEm (io:ios) = do
    --         io
    --         runEm ios
    --       runEm [] = return ()

launchIOs :: [IO ()] -> IO ()
launchIOs = mapM_ forkIO

_calls = mkFielder "_calls" calls $ \w a -> w { calls = a }
