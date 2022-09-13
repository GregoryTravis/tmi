{-# Language GADTs #-}

module TMI
( cps
, vcps
, call ) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

import Lift
import Ty
import Util
import VReadShow

instance Functor (TMI w) where
  fmap = liftM

instance Applicative (TMI w) where
  pure  = return
  (<*>) = ap

instance Monad (TMI w) where
  (>>=) = Bind
  return = Step . Ret

call :: (Read a, Show a) => IO a -> TMI w a
call = Step . Ext

-- Convert a TMI to a CPS

cps :: (Read a, Show a) => TMI w a -> CPS w a
cps tmi = cps' tmi (\_ -> Done)

vcps :: (Read a, Show a) => V w (TMI w a) -> V w (CPS w a)
vcps = ulift1 "cps" cps

cps' :: TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
cps' (Bind (Step a) k') k = KBind a (\a -> cps' (k' a) k)
cps' (Bind b@(Bind _ _) k') k = cps' b (\a -> cps' (k' a) k)

instance Show (TMI w a) where
  show (Step step) = "(Step " ++ (show step) ++ ")"
  show (Bind tmi' k) = "(Bind " ++ (show tmi') ++ " ...k)"

instance Show (CPS w a) where
  show (KBind step k) = "(KBind " ++ (show step) ++ " ...k)"
  show Done = "Done"

instance Show (Step w a) where
  show (Ext x) = "(Ext _)"
  show (Ret a) = "(Ret _)"
  show (WriteStep (Write va a)) = "(WriteStep " ++ show va ++ ")"
