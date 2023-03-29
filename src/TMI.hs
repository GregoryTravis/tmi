{-# Language GADTs #-}

module TMI
( cps
, vcps
, call
, (<--)
, (<--*) ) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import System.IO.Unsafe

import Lift
import Tracer
import Ty
import Util
import VReadShow

doTraceCps :: Bool
doTraceCps = False

instance Functor (TMI w) where
  fmap = liftM

instance Applicative (TMI w) where
  pure  = return
  (<*>) = ap

instance Monad (TMI w) where
  (>>=) = Bind
  return = Step . Ret

-- TODO no idea how to implement this
instance MonadFail (TMI w) where
  fail s = do call $ msp "TMI MonadFail fail"
              return undefined

call :: (Read a, Show a) => IO a -> TMI w a
call = Step . Ext

cps :: (Read a, Show a) => TMI w a -> TMI w ()
cps tmi = ensureCPS $ traceCpsMaybe $ cps' tmi (\_ -> Done)

vcps :: (Read a, Show a) => V w (TMI w a) -> V w (TMI w ())
vcps = ulift1 "cps" cps

cps' :: TMI w a -> (a -> TMI w b) -> TMI w b
-- TODO why not use the orig k instead of the extra Done k? And why doesn't that work?
cps' (Step (CallCC kr)) k = Bind (Step (CallCC kr')) k
  where kr' k = cps' (kr k) (\() -> Done)
cps' (Step (Fork tmi)) k = Bind (Step (Fork tmi')) k
  where tmi' = cps tmi
cps' (Step a) k = Bind (Step a) k
cps' (Bind b k') k = cps' b (kcps k' k)
-- TODO very questionable, why does Done have a continuation?
cps' Done k = expectDone $ k ()

-- Count steps and emit report at the end -- lazy wrapper
-- Am I a terribly bad person?
-- This is just a poc, it should count and report
traceCps :: TMI w a -> TMI w a
traceCps tmi = unsafePerformIO $ do
  lt <- mkListTracer "traceCps"
  return $ traceCps' lt tmi

traceCps' :: (Maybe String -> IO ()) -> TMI w a -> TMI w a
traceCps' lt (Bind (Step step) k) = unsafePerformIO $ do
  lt (Just $ show step)
  return $ Bind (Step step) k'
  where k' a = traceCps' lt (k a)
traceCps' lt Done = unsafePerformIO $ do
  lt Nothing
  return Done

traceCpsMaybe :: TMI w a -> TMI w a
traceCpsMaybe = if doTraceCps then traceCps else id

expectDone :: TMI w a -> TMI w a
expectDone Done = Done
expectDone x = error $ "expectDone: not done, but " ++ show x

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
  show (WriteStep write) = "(WriteStep " ++ show write ++ ")"
  show (CallCC _) = "(CallCC _)"
  show (Fork _) = "(Fork _)"
  show (Read v) = "(Read " ++ show v ++ ")"
  show (Log s) = "(Log " ++ s ++ ")"
  show (Freeze v) = "(Freeze " ++ show v ++ ")"

infixl 1 <--*
(<--*) :: V w a -> a -> TMI w ()
va <--* a = Step $ WriteStep (Write va a)

infixl 1 <--
(<--) :: V w a -> V w a -> TMI w ()
va <-- va' = Step $ WriteStep (VWrite va va')
