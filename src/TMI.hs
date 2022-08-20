module TMI
( cps
, vcps ) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

import Lift
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

vcps :: (Read a, Show a) => V w (TMI w a) -> V w (CPS w a)
vcps = ulift1 "cps" cps

cps' :: (Read a, Show a) => TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
cps' (Bind (Step a) k') k = KBind a (\a -> cps' (k' a) k)
-- TODO handle case of a bind with a bind on the left
