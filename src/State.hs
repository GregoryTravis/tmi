{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module State
( listen
, (<--)
, (<--.)
, tmiRun
) where

import Control.Monad.State hiding (lift)

import History
import Internal
import Lift

infix 4 <--.
(<--.) :: Nice a => V a -> a -> TMI h w ()
vlvalue <--. rvalue = vlvalue <-- konstV rvalue

infix 4 <--
(<--) :: Nice a => V a -> V a -> TMI h w ()
vlvalue <-- vrvalue = do
  history <- get
  rvalue <- rd vrvalue
  let wr = Write (dyv vlvalue) (dy rvalue)
  history' <- liftIO $ write history [wr]
  put history'
  return ()

mkListener :: Nice a => V a -> (a -> IO ()) -> Listener
mkListener v action = Listener {..}
  where getDv = dyv v
        runReader :: Reader -> IO ()
        runReader reader = do
          a <- unReader reader v
          action a

listen :: Nice a => V a -> (a -> IO ()) -> TMI h w ()
listen v action = do
  history <- get
  let listener = mkListener v action
      history' = addListener history listener
  put history'

-- dump :: TMI h w ()
-- dump = do
--   history <- get
--   liftIO $ runListeners history

tmiRun :: (Nice w, History h w) => w -> TMI h w a -> IO (a, h w)
tmiRun w action = do
  let history = mkHistory w
  runStateT action history

rd :: (Nice a, Nice w, History h w) => V a -> TMI h w a
rd v = do
  history <- get
  a <- liftIO $ readV history v
  return a
