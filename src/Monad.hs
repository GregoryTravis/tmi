module Monad where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

import Core
import Util

instance Functor (Blef w) where
  fmap = liftM

instance Applicative (Blef w) where
  pure  = return
  (<*>) = ap

instance Monad (Blef w) where
  -- (>>=) :: Blef w a -> (a -> Blef w b) -> Blef w b
  (>>=) = boond
  -- return :: a -> Blef w a
  return = BReturn

instance MonadFail (Blef w) where
  fail s = error $ "Fail " ++ s

-- (>>) :: Blef w a -> Blef w b -> Blef w b
-- ba >> bb = ba >>= \_ -> bb

-- fail :: String -> Blef w a
-- fail s = error $ "tmi monad fail " ++ s

io :: (Read a, Show a) => IO a -> Blef w a
io action = Blef "" action

commit :: Blef w ()
commit = do
  () <- io $ return ()
  return ()
