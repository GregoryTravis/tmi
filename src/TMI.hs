{-# Language GADTs #-}

module TMI
( cps
, vcps
, call
, (<--)
, (<--*) ) where

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

cps :: (Read a, Show a) => TMI w a -> TMI w ()
cps tmi = ensureCPS $ cps' tmi (\_ -> Done)

vcps :: (Read a, Show a) => V w (TMI w a) -> V w (TMI w ())
vcps = ulift1 "cps" cps

cps' :: TMI w a -> (a -> TMI w b) -> TMI w b
cps' (Step (CallCC kr)) k = Bind (Step (CallCC kr')) k
  where kr' k = cps' (kr k) (\() -> Done)
cps' (Step a) k = Bind (Step a) k
cps' (Bind b k') k = cps' b (kcps k' k)
-- TODO very questionable
cps' Done k = k ()

kcps :: (a -> TMI w b) -> (b -> TMI w c) -> (a -> TMI w c)
kcps tk ck = \a -> cps' (tk a) ck

ensureCPS :: TMI w () -> TMI w ()
ensureCPS x@(Bind (Step step) k) = {-eeesp "ensureCPS" $-} Bind (Step step) (ensureKCPS k)
ensureCPS x@Done = {-eeesp "ensureCPS" $-} Done
ensureCPS x = error $ "ensureCPS " ++ show x

ensureKCPS :: (a -> TMI w ()) -> (a -> TMI w ())
ensureKCPS k = \a -> ensureCPS (k a)

instance Show (TMI w a) where
  show (Step step) = "(Step " ++ (show step) ++ ")"
  show (Bind tmi' k) = "(Bind " ++ (show tmi') ++ " ...k)"
  show Done = "Done"

instance Show (Step w a) where
  show (Ext x) = "(Ext _)"
  show (Ret a) = "(Ret _)"
  show (CallCC _) = "(CallCC _)"
  show (WriteStep (Write va a)) = "(WriteStep " ++ show va ++ ")"

infixl 1 <--*
(<--*) :: V w a -> a -> TMI w ()
va <--* a = Step $ WriteStep (Write va a)

infixl 1 <--
(<--) :: V w a -> V w a -> TMI w ()
va <-- va' = Step $ WriteStep (VWrite va va')
