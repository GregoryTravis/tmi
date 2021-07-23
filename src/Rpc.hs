{-# LANGUAGE NamedFieldPuns, NumericUnderscores, RecordWildCards #-}

module Rpc
( Ext(..)
, Req(..)
, Resp(..)
, Rpc(..)
, Call(..)
, initRpc
, refreshRpcs ) where

import Control.Concurrent (forkIO, threadDelay)

import ExecId
import UniqueId
import Util

newtype Ext a = Ext (IO a)

data Initiation = Initiation ExecId deriving (Eq, Show)

data Req = Req Float String deriving Show
data Resp = Resp String deriving Show

-- -- Sample one
-- reqToIO :: String -> Req -> IO Req
-- reqToIO extry (Req secs s) = do
--   threadDelay (floor $ s * 1_000_000)
--   putStrLn s
--   return $ s ++ " after " ++ (show s) ++ extry

data Rpc = Rpc
  { calls :: [Call]
  , toExt :: Req -> IO Resp
  -- , toTmi :: Resp -> TMI WW ()
  }

instance Show Rpc where
  -- TODO do not love this
  show rpc = "RPC " ++ show (calls rpc)

initRpc :: Rpc
initRpc = Rpc [] toExt -- toTmi
  where toExt r@(Req secs s) = io
          where io = do
                  threadDelay $ floor $ secs * (fromIntegral 1_000_000)
                  msp $ "Running ext: " ++ show r
                  return $ Resp $ s ++ "!"
        -- toTmi (Resp s) = do
        --   liftIO $ msp $ "Consequence: " ++ s

data Call = Call
  { callUniqueId :: UniqueId
  , req :: Req
  , initiation :: Maybe Initiation
  , resp :: Maybe Resp
  , consquenceEnacted :: Bool
  } deriving Show

clearOutStaleInitiations :: ExecId -> [Call] -> [Call]
clearOutStaleInitiations execId calls = map (clearOutStaleInitiation execId) calls
  where clearOutStaleInitiation currentExecId
                                c@(Call { initiation = Just (Initiation execId) } )
          | execId == currentExecId = c
          | otherwise = c { initiation = Nothing }
        clearOutStaleInitiation _ c = c

requestsThatNeedInitiation :: [Call] -> [Call]
requestsThatNeedInitiation = filter needInitiation
  where needInitiation call = initiation call == Nothing

refreshRpcs :: ExecId -> Rpc -> IO Rpc
refreshRpcs execId rpc@(Rpc {..})  = do
  -- Clear out stale initiations
  let calls' = clearOutStaleInitiations execId calls
      -- Get requests that need initiation
      needInit :: [Call]
      needInit = requestsThatNeedInitiation calls'
      -- Initiate them
      ios = map toIOV (map req calls')
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
        where setInit c@(Call { callUniqueId = uid }) | uid `elem` uids =
                c { initiation = Just init }
  launchIOs ios
  return rpc
    -- where runEm (io:ios) = do
    --         io
    --         runEm ios
    --       runEm [] = return ()

launchIOs :: [IO ()] -> IO ()
launchIOs ios = mapM_ forkIO ios
