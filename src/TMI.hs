module TMI
( cps ) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

import Ty
import Util

instance Functor (TMI w) where
  fmap = liftM

instance Applicative (TMI w) where
  pure  = return
  (<*>) = ap

instance Monad (TMI w) where
  (>>=) = Bind
  return = Step . Ret

-- Convert a TMI to a CPS

cps :: (Read a, Show a) => TMI w a -> CPS w a
cps tmi = cps' tmi (\_ -> Done)

cps' :: (Read a, Show a) => TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
cps' (Bind (Step a) k') k = KBind a (\a -> cps' (k' a) k)
-- TODO handle case of a bind with a bind on the left
