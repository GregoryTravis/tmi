{-# LANGUAGE GADTs, RankNTypes #-}

module Monad
( Step(..)
, Core(..)
, Program(..)
, applyContinuation
, Monitor(..)
, Monitoring(..)
) where

import Util
import V
import Ty

-- The core language consists of steps, each with its continuation.
-- data Core w where
--   Assign :: Write w -> (() -> Core w) -> Core w
--   Call :: forall a w. IO a -> (a -> Core w) -> Core w
--   Done :: Core w

type Monitor a = (a -> IO ())
data Monitoring w = forall a. Monitoring (V w a) (Monitor a)

data Step w = forall a. (Show a, Read a) => Step (IO a) (a -> Program w)
data Core w = Assign (Write w) | Call (Step w) | Mon (Monitoring w) | Sub (Program w) |
  Cond (V w Bool) (Core w) (Core w) | Done
data Program w = Program [Core w] deriving Show

instance Show (Core w) where
  show (Assign write) = "(Assign " ++ show write ++ ")"
  show (Call _) = "(Call)"
  show (Mon (Monitoring v _)) = "(Monitor " ++ show v ++ ")"
  show (Sub program) = "(Sub " ++ show program ++ ")"
  show (Cond vb _ _) = "(Cond " ++ show vb ++ ")"
  show Done = "(Done)"

applyContinuation :: Step w -> String -> Program w
applyContinuation (Step _ k) s = k (read s)

-- data Step w = forall a. Read a => Step (IO a) (a -> Core w)
