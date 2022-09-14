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

-- cps2tmi :: CPS w a -> TMI w a
-- cps2tmi (KBind step ck) = Bind (Step step) (cpsk2tmik ck)

-- cpsk2tmik :: (a -> CPS w b) -> (a -> TMI w b)
-- cpsk2tmik ck = \a -> cps2tmi (ck a)

-- Convert a TMI to a CPS

cps :: (Read a, Show a) => TMI w a -> CPS w ()
cps tmi = cps' tmi (\_ -> Done)

vcps :: (Read a, Show a) => V w (TMI w a) -> V w (CPS w ())
vcps = ulift1 "cps" cps

cps' :: TMI w a -> (a -> CPS w b) -> CPS w b
cps' (Step a) k = KBind a k
-- -- Step a :: TMI a
-- -- k' :: a -> TMI b
-- -- k :: b -> TMI c
-- -- cps' (Bind (Step a) k') k = cps' (Step a) (composeKs k' k)

-- Replaced the two with the one
-- cps' (Bind (Step a) k') k = KBind a (\a -> cps' (k' a) k)
-- cps' (Bind b@(Bind _ _) k') k = cps' b (\a -> cps' (k' a) k)
cps' (Bind b k') k = cps' b (\a -> cps' (k' a) k)

-- TODO or is this join?
composeKs :: (a -> TMI w b) -> (b -> TMI w c) -> (a -> TMI w c)
composeKs a2b b2c = \a -> a2b a >>= b2c

-- -- TODO or is this join?
-- composeCKs :: (a -> CPS w b) -> (b -> CPS w c) -> (a -> CPS w c)
-- composeCKs a2b b2c = \a -> a2b a >>= b2c

-- cps' (CallCC tkr) ck = KCallCC ckr
--   where ckr = \ck -> let tk = cpsk2tmik ck
--                       in cps (tkr tk)

-- cps' (Bind (CallCC kr) k') k = cps' (kr k') k
-- cps' (Bind (CallCC kr) k') k = cps' (kr k') k

-- cps' (CallCC kr) k = error "bare CallCC"

-- cps' (CallCC kr) k = KCallCC ckr ck -- ? not sure about ck needing to be here

-- cps' (CallCC kr) k = KCallCC ckr ck -- ? not sure about ck needing to be here
--   where ckr :: (a -> CPS w ()) -> CPS w ()
--         ckr q =
--           let q :: a -> CPS w ()
--               \a -> 

-- helpy :: (a -> TMI w b) -> (a -> CPS w b)
-- helpy tk = \a -> cps (tk a)

-- cps' (CallCC kr) k =
--   let kr :: (a -> TMI w ()) -> TMI w ()
--       k :: (a -> CPS w b)
--       x :: TMI w ()
--       x = kr q
--       q :: (a -> TMI w ())
--       q = \a -> 

-- kr :: ((a -> TMI w ()) -> TMI w ())
-- cps' (CallCC kr) k =
--   let kr :: ((a -> TMI w ()) -> TMI w ())
--       kr (\tk :: (a -> TMI w ())
--            (\a -> cps' (tk a)
-- cps' (CallCC kr) k = cps' (kr' k) (\_ -> Done)
--   where kr' a = cps' (kr a)

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
