{-# Language NamedFieldPuns #-}

module Runtime
( mainLoop ) where

import Control.Monad.State.Lazy
import Lift
import Ty
import Util

type St w a = StateT (H w) IO a

addTodo :: V w (CPS w ()) -> St w ()
addTodo tmi = do
  h@H { todo } <- get
  put $ h { todo = todo ++ [tmi] }

-- Nixing this because what I want is a Maybe V, not a V Maybe
-- stepTmi :: CPS w () -> Maybe (CPS w ())
-- stepTmi (KBind (Ret a) k) = Just (k a)
-- stepTmi _ = error "unimplemented stepTmi"
-- vStepTmi :: V w (CPS w ()) -> V w (Maybe (CPS w ()))
-- vStepTmi = ulift1 "stepTmi" stepTmi

-- Nixing this because it would require a read
stepTmi :: V w (CPS w ()) -> Maybe (V (CPS w ()))

-- runThisTmi Done = return ()
-- runThisTmi (KBind (Ext io) k) = do
--   -- Just gonna run it right here, TODO don't do that
--   a <- liftIO io
--   -- ignore k
--   return Nothing

runATmiTodo :: St w ()
runATmiTodo = return ()
-- runATmiTodo = do
--   h@H { todo } <- get
--   case todo of [] -> return ()
--                (tmi:tmis) -> do case runThisTmi tmi of Nothing -> put $ h { todo = tmis }
--                                                        Just vcps -> put $ h { todo = vcons vcps tmis }

step :: St w ()
step = do
  -- Handle new retvals
  -- Start new calls
  -- Run a todo TMI
  runATmiTodo
  liftIO $ msp "runtime mainLoop Hi"

mainLoop :: H w -> IO ()
mainLoop h = do
  liftIO $ msp "mainLoop start"
  ((), h') <- runStateT step h
  liftIO $ msp "mainLoop done"
