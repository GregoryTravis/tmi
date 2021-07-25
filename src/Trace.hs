module Trace where

initCall :: Req -> TMI WW Call
initCall req = do
  uid <- uniqueId
  return $ Call uid req Nothing Nothing False

initRpc :: Rpc
initRpc = Rpc [] toExt -- toTmi
  where toExt r@(Req secs s) = io
          where io = do
                  threadDelay $ floor $ secs * (fromIntegral 1_000_000)
                  msp $ "Running ext: " ++ show r
                  return $ Resp $ s ++ "!"
        -- toTmi (Resp s) = do
        --   liftIO $ msp $ "Consequence: " ++ s

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
