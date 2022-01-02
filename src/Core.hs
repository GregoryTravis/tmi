{-# LANGUAGE GADTs, RankNTypes #-}

module Core
( Call(..)
, Event(..)
, Core(..)
, Program(..)
, applyContinuation
, Monitor(..)
, Monitoring(..)
, wrapAction
, Blef(..)
, toProg
, boond
, ritt
, done
, sdone
) where

import Control.Concurrent

import Util
import V
import Ty

type Monitor a = (a -> IO ())
data Monitoring w = forall a. Monitoring (V w a) (Monitor a)

data Call w = forall a. (Show a, Read a) => InternalCall String (IO a) (a -> Program w)
            | forall a. (Show a, Read a) => ExternalCall String (a -> Program w)
data Event w = Retval Int String | Command [String] deriving (Show, Read)
data Core w = Assign (Write w) | Call (Call w) | Sub (Program w)
            | Cond (V w Bool) (Core w) (Core w) | Named String (Core w) | Done
data Program w = Program [Core w] deriving Show

instance Show (Core w) where
  show (Assign write) = "(Assign " ++ show write ++ ")"
  show (Call _) = "(Call)"
  show (Sub program) = "(Sub " ++ show program ++ ")"
  show (Cond vb _ _) = "(Cond " ++ show vb ++ ")"
  -- show (Named s c) = "(Named " ++ show s ++ " " ++ show c ++ ")"
  show Done = "(Done)"

instance Show (Call w) where
  show (InternalCall s _ _) = "(InternalCall " ++ s ++ ")"
  show (ExternalCall s _) = "(ExternalCall " ++ s ++ ")"

applyContinuation :: Call w -> String -> Program w
applyContinuation (InternalCall _ _ k) s = k (read s)
applyContinuation (ExternalCall _ k) s = k (read s)

-- Wrap an action to 'show' its value into a retval and send it down a channel.
-- Only for InternalCalls.
wrapAction :: Chan (Event w) -> Int -> Call w -> IO ()
wrapAction chan index (InternalCall _ io _) = do
  a <- io
  let as = show a
      retval = Retval index as
  -- msp $ "wrapAction chan write: " ++ show retval
  writeChan chan retval
  return ()
wrapAction _ _ _ = error "Cannot wrap a non-InternalCall"

-- Monad stuff
-- Hmm it actually desugars to being right associative??
data Blef a = Blef String (IO a) | forall b. (Show a, Read a, Show b, Read b) => Blefs (Blef b) (b -> Blef a)

instance Show (Blef a) where
 show (Blef s _) = "(Blef " ++ s ++ ")"
 show (Blefs b a2b) = "(Blefs " ++ show b ++ ")"

boond :: (Show a, Read a, Show b, Read b) => Blef a -> (a -> Blef b) -> Blef b
-- TODO make a constructor that only allows the correct usage pattern, infixl
boond = Blefs

ritt :: (Show a, Read a) => IO a -> Blef a
ritt = Blef "ritt"

-- Presumably this machinery could be somehow folded in to the type so it doesn't
-- have to be free-ish.
toProg :: (Show a, Read a) => (a -> Program w) -> Blef a -> Program w
toProg k (Blef s io) =
  Program [Call $ InternalCall s io k]
toProg k (Blefs blef a2Blef) =
  toProg (\a -> toProg k (a2Blef a)) blef

done :: a -> Program w
done _ = Program [Done]
sdone :: Show a => a -> Program w
sdone a = Program [Call $ InternalCall "" (msp ("sdone", a)) done]
