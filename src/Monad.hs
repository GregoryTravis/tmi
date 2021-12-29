{-# LANGUAGE GADTs, RankNTypes #-}

module Monad
( Call(..)
, Event(..)
, Core(..)
, Program(..)
, applyContinuation
, Monitor(..)
, Monitoring(..)
, wrapAction
) where

import Control.Concurrent

import Util
import V
import Ty

type Monitor a = (a -> IO ())
data Monitoring w = forall a. Monitoring (V w a) (Monitor a)

data Call w = forall a. (Show a, Read a) => InternalCall (IO a) (a -> Program w)
            | forall a. (Show a, Read a) => ExternalCall (a -> Program w)
data Event w = Retval Int String | Command [String] deriving (Show, Read)
data Core w = Assign (Write w) | Call (Call w) | Sub (Program w)
            | Cond (V w Bool) (Core w) (Core w) | Done
data Program w = Program [Core w] deriving Show

instance Show (Core w) where
  show (Assign write) = "(Assign " ++ show write ++ ")"
  show (Call _) = "(Call)"
  show (Sub program) = "(Sub " ++ show program ++ ")"
  show (Cond vb _ _) = "(Cond " ++ show vb ++ ")"
  show Done = "(Done)"

applyContinuation :: Call w -> String -> Program w
applyContinuation (InternalCall _ k) s = k (read s)
applyContinuation (ExternalCall k) s = k (read s)

-- Wrap an action to 'show' its value into a retval and send it down a channel.
-- Only for InternalCalls.
wrapAction :: Chan (Event w) -> Int -> Call w -> IO ()
wrapAction chan index (InternalCall io _) = do
  a <- io
  let as = show a
      retval = Retval index as
  writeChan chan retval
  return ()
wrapAction _ _ _ = error "Cannot wrap a non-InternalCall"
