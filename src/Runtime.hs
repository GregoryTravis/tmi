{-# Language NamedFieldPuns #-}

module Runtime
( mainLoop ) where

import Control.Monad.State.Lazy

import qualified CoatCheck as CC
import H
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
runATodo :: St w ()
runATodo = do
  h@H { calls, todo, generations } <- get
  let latestW = last generations
  case todo of
    [] -> return ()
    (vcps:vcpss) -> do case stepCPS latestW vcps of
                        Stepped vcps' -> put $ h { todo = vcps' : vcpss }
                        Called vcps -> do
                          let (tag, cc') = CC.check calls vcps
                          -- use tag
                          put $ h { calls = cc', todo = vcpss }
                        -- Called (CPS (Ext io) k) -> do retval <- liftIO io
                        --                                let retvalS = show retval
                        --                                    vcps' = advanceExtBindV vcps (VNice retvalS)
                        --                                put $ h { todo = vcps' : vcpss }
                        Nada -> put $ h { todo = vcpss }

step :: Show w => St w ()
step = do
  atd <- anythingToDo
  if not atd
    then return ()
    else do
           -- Handle new retvals
           -- Start new calls
           -- Run a todo TMI
           doLog
           runATodo
           doLog
           -- liftIO $ msp "runtime mainLoop Hi"
           step
  where showHistory = get >>= (liftIO . msp)
        showNextTodo :: St w ()
        showNextTodo = do
          h@H { todo } <- get
          case todo of
            [] -> liftIO $ msp "no todos"
            (todo:todos) -> liftIO $ msp $ rd (last (generations h)) todo
        verbose = False
        doLog = do
          if verbose
            then do
                   showHistory
                   showNextTodo
            else return ()
        anythingToDo :: St w Bool
        anythingToDo = do
          H { todo } <- get
          return $ not $ null todo

mainLoop :: Show w => H w -> IO ()
mainLoop h = do
  -- liftIO $ msp "mainLoop start"
  ((), h') <- runStateT step h
  -- liftIO $ msp "mainLoop done"
  return ()
