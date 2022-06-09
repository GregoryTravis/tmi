module TMI
( cps ) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

import Ty
import Util

-- instance Functor (TMI w) where
--   fmap = liftM

-- instance Applicative (TMI w) where
--   pure  = return
--   (<*>) = ap

-- bernd ::(Read a, Show a, Read b, Show b) =>  TMI w b -> (b -> TMI w a) -> TMI w a
-- bernd = Bind

-- instance Monad (TMI w) where
--   (>>=) = bernd
--   return = Step . Ret

cps :: (Read a, Show a) => TMI w a -> CPS w a
cps tmi = cps' tmi (\_ -> Done)

cps' :: (Read a, Show a) => TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
cps' (Bind (Step a) k') k = KBind a (\a -> cps' (k' a) k)
