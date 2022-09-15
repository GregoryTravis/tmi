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

-- -- Convert a TMI to a CPS
-- jps :: (Read a, Show a) => TMI w a -> J w
-- jps tmi = jps' tmi (\_ -> JDone)

-- jps' :: TMI w a -> (a -> J w) -> J w
-- jps' (Step a) k = JBind a k
-- jps' (Bind b k') k = jps' b (jkcps k' k)
-- -- jps' (CallCC tkr) k = JCallCC k'
-- --   where k' = \

-- jkcps :: (a -> TMI w b) -> (b -> J w) -> (a -> J w)
-- jkcps tk ck = \a -> jps' (tk a) ck

-- Convert a TMI to a CPS
cps :: (Read a, Show a) => TMI w a -> CPS w ()
cps tmi = cps' tmi (\_ -> Done)

vcps :: (Read a, Show a) => V w (TMI w a) -> V w (CPS w ())
vcps = ulift1 "cps" cps

cps' :: TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
-- cps' (Bind b k') k = cps' b (\a -> cps' (k' a) k)
cps' (Bind b k') k = cps' b (kcps k' k)

-- cps' (CallCC tkr) ck = KCallCC (ugh3 tkr)
-- (CallCC tkr) :: TMI w a
-- ck :: a -> CPS w b
-- tkr :: (a -> TMI w ()) -> TMI w ()

ugh :: CPS w a -> TMI w a
ugh (KBind step k) = Bind (Step step) k'
  where k' = \a -> ugh (k a)

ugh2 :: (a -> CPS w ()) -> (a -> TMI w ())
ugh2 ck = \a -> ugh (ck a)

ugh3 :: ((a -> TMI w ()) -> TMI w ()) -> ((a -> CPS w ()) -> CPS w ())
ugh3 tkr = \ckr -> cps $ tkr (ugh2 ckr)

-- a <- CallCC (\a2b -> a2b a)
-- doStuff a :: TMI b
--    aka
-- Bind (CallCC (\a2b -> a2b a)) (a -> doStuff a)

-- ((a -> TMI w b) -> TMI w b) -> ((a -> CPS w b) -> CPS w b)

kcps :: (a -> TMI w b) -> (b -> CPS w c) -> (a -> CPS w c)
kcps tk ck = \a -> cps' (tk a) ck

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

infixl 1 <--*
(<--*) :: V w a -> a -> TMI w ()
va <--* a = Step $ WriteStep (Write va a)

infixl 1 <--
(<--) :: V w a -> V w a -> TMI w ()
va <-- va' = Step $ WriteStep (VWrite va va')
