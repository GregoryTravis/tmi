{-# Language NamedFieldPuns #-}

module Runtime
( mainLoop ) where

import Control.Monad.State.Lazy

import CoatCheck hiding (null)
import qualified CoatCheck
import ExtRunner
import H
import Lib
import Lift
import Propagate
import Ty
import Util

verbose = False

type St w a = StateT (H w) IO a

showHistory :: Show w => St w ()
showHistory = get >>= (liftIO . msp)

showNextTodo :: St w ()
showNextTodo = do
  h@H { todo } <- get
  case todo of
    [] -> liftIO $ msp "no todos"
    (todo:todos) -> liftIO $ msp $ rd (last (generations h)) todo

doLog :: Show w => St w ()
doLog = do
  if verbose
    then do
           showHistory
           showNextTodo
    else return ()

addTodo :: V w (CPS w ()) -> St w ()
addTodo tmi = do
  h@H { todo } <- get
  put $ h { todo = todo ++ [tmi] }

-- stepTmi :: w -> V w (CPS w ()) -> Maybe (V w (CPS w ()))
-- stepTmi w vcps =
--   let cps = rd w vcps
--    in case cps of
--         (KBind (Ret _) _) -> Just (advanceRetBindV vcps)
--         Done -> Nothing

data StepResult w = Stepped (V w (CPS w ())) | Called (V w (CPS w ())) | Nada

stepCPS :: w -> V w (CPS w ()) -> StepResult w
stepCPS w vcps =
  let cps = rd w vcps
   in case cps of
        (KBind (Ret _) _) -> Stepped (advanceRetBindV vcps)
        (KBind e@(Ext _) k) -> Called vcps
        Done -> Nada

-- Resolve a Ret immediately
advanceRetBind :: CPS w b -> CPS w b
advanceRetBind (KBind (Ret x) k) = k x

advanceRetBindV :: V w (CPS w b) -> V w (CPS w b)
advanceRetBindV = ulift1 "advanceRetBind" advanceRetBind

-- Resolve a call with the retval string
advanceExtBind :: CPS w b -> String -> CPS w b
advanceExtBind (KBind (Ext _) k) retvalS = k (read retvalS)

advanceExtBindV :: V w (CPS w b) -> V w String -> V w (CPS w b)
advanceExtBindV = ulift2 "advanceExtBind" advanceExtBind

-- TODO don't run an Ext immedaitely
runATodo :: ExtRunner (Tag, String) -> St w ()
runATodo er = do
  h@H { calls, todo, generations } <- get
  let latestW = last generations
  case todo of
    [] -> return ()
    (vcps:vcpss) -> do case stepCPS latestW vcps of
                        Stepped vcps' -> put $ h { todo = vcps' : vcpss }
                        Called vcps -> do
                          let (tag, cc') = check calls vcps
                          liftIO $ startCall er latestW tag vcps
                          put $ h { calls = cc', todo = vcpss }
                        -- Called (CPS (Ext io) k) -> do retval <- liftIO io
                        --                                let retvalS = show retval
                        --                                    vcps' = advanceExtBindV vcps (VNice retvalS)
                        --                                put $ h { todo = vcps' : vcpss }
                        Nada -> put $ h { todo = vcpss }

externalize :: (CPS w ()) -> Tag -> IO (Tag, String)
externalize (KBind (Ext ioa) _) tag = do
  msp "RUNNING IOTS"
  a <- ioa
  let s = show a
  return (tag, s)

startCall :: ExtRunner (Tag, String) -> w -> Tag -> V w (CPS w ()) -> IO ()
startCall er w tag vcps = do
  let cps = rd w vcps
      iots = externalize cps tag
  run er iots

waitForRetval :: ExtRunner (Tag, String) -> St w ()
waitForRetval er = do
  h@H { calls, todo } <- get
  (tag, retvalS) <- liftIO $ nextResult er
  liftIO $ msp $ "Retrieve " ++ retvalS ++ " for " ++ show tag
  let Just (vcps, calls') = retrieve calls tag
      vcps' = advanceExtBindV vcps (VNice retvalS)
      h' = h { calls = calls', todo = vcps':todo }
  put h'

loop :: Show w => ExtRunner (Tag, String) -> St w ()
loop er = do
  atd <- anythingToDo
  if not atd
    then do
      aosc <- anyOutStandingCalls
      if aosc
        then do
          liftIO $ msp "wait for retval"
          waitForRetval er
        else do
          liftIO $ msp "Nothing to do and nothing doing"
          return ()
    else do
      -- Handle new retvals
      -- Start new calls
      -- Run a todo TMI
      liftIO $ msp "there's a todo"
      doLog
      runATodo er
      doLog
      loop er

anythingToDo :: St w Bool
anythingToDo = do
  H { todo } <- get
  return $ not $ null todo

anyOutStandingCalls :: St w Bool
anyOutStandingCalls = do
  H { calls } <- get
  return $ not $ CoatCheck.null calls

mainLoop :: Show w => H w -> IO ()
mainLoop h = do
  er <- mkExtRunner
  runStateT (loop er) h
  msp "loop done"
  return ()
