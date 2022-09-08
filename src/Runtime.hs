{-# Language NamedFieldPuns #-}

module Runtime
( mainLoop ) where

import Control.Monad.State.Lazy
import Lib
import Lift
import Propagate
import Ty
import Util

type St w a = StateT (H w) IO a

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

data StepResult w = Stepped (V w (CPS w ())) | Called (Call w) | Nada

stepCPS :: w -> V w (CPS w ()) -> StepResult w
stepCPS w vcps =
  let cps = rd w vcps
   in case cps of
        (KBind (Ret _) _) -> Stepped (advanceRetBindV vcps)
        (KBind e@(Ext _) k) -> Called (Call e k)
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
runATodo :: St w ()
runATodo = do
  h@H { todo, generations } <- get
  let latestW = last generations
  case todo of
    [] -> return ()
    (vcps:vcpss) -> do case stepCPS latestW vcps of
                        Stepped vcps' -> put $ h { todo = vcps' : vcpss }
                        Called (Call (Ext io) k) -> do retval <- liftIO io
                                                       let retvalS = show retval
                                                           vcps' = advanceExtBindV vcps (VNice retvalS)
                                                       put $ h { todo = vcps' : vcpss }
                        Nada -> put $ h { todo = vcpss }

step :: St w ()
step = do
  -- Handle new retvals
  -- Start new calls
  -- Run a todo TMI
  runATodo
  liftIO $ msp "runtime mainLoop Hi"

mainLoop :: H w -> IO ()
mainLoop h = do
  liftIO $ msp "mainLoop start"
  ((), h') <- runStateT step h
  liftIO $ msp "mainLoop done"
