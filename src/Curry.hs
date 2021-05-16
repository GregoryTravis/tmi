{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs, MultiParamTypeClasses,
             RankNTypes, StandaloneDeriving #-}

module Curry
( W(..)
, R(..)
, Write
, V(..)
, Receiver(..)
, hybrid1
, hybrid2
, (<--)
, (<**>)
, (<$$>)
, History(..)
, SimpleHistory
, TMI
, tmiRun
, (<---)
, listen
) where

import Control.Monad.Cont
import Control.Monad.State.Lazy
import Data.Dynamic
import Data.Maybe
import Data.Proxy

import Util

data W = W { anInt :: Int, anotherInt :: Int }
  deriving Show

-- data Write1 = forall a. Write1 a
data Write1 = forall a. Show a => Write1 (V a) a
deriving instance Show Write1
data Write = Write [Write1] deriving Show
instance Semigroup Write where
  Write ws <> Write ws' = Write $ ws ++ ws'
data Receiver a = Receiver String (a -> Write)
instance Show (Receiver a) where
  show (Receiver s _) = "REC Receiver " ++ s
-- data Receiver a = Receiver (V a)
data R a = R a (Receiver a)
  deriving Show
infix 1 <--
(<--) :: Receiver a -> a -> Write
Receiver s r <-- x = {-eeesp ("REC <-- call", s) $-} r x
-- Receiver va <-- x = Write [Write1 va x]
-- Receiver va <-- x = Write [Write1 va x]

hybrid1 :: (a -> b) -> (R a -> b -> Write) -> (R a -> R b)
hybrid1 f r ra@(R x rx) = R x' rx'
  where x' = f x
        rx' = Receiver "hybrid1" $ \x -> r ra x

hybrid2 :: (a -> b -> c) -> (R a -> R b -> c -> Write) -> (R a -> R b -> R c)
hybrid2 f r ra@(R x rx) rb@(R y ry) = R z rz
  where z = f x y
        rz = Receiver "hybrid2" $ \x -> r ra rb x

data V a where
  VRoot :: Typeable a => V a
  VConst :: (Show a, Typeable a) => a -> V a
  VPartialApp :: (Show a, Typeable a) => V (R a -> rest) -> V a -> V rest
  VApp :: (Typeable a, Typeable b, Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a, Typeable a) => V (R a) -> V a

-- more succinct
k :: (Show a, Typeable a) => a -> V a
k = VConst
infixl 4 <**>
(<**>) :: (Show a, Typeable a) => V (R a -> rest) -> V a -> V rest
(<**>) = VPartialApp
infixl 4 <$$>
(<$$>) :: (Typeable a, Typeable b, Show a, Show b) => V (R b -> R a) -> V b -> V a
(<$$>) = VApp

-- app2 :: V (R a -> R b -> R c) -> V a -> V (R b -> R c)
-- partialApp :: V (R a -> rest) -> V a -> V rest
-- partialApp = undefined

instance Show (a -> b) where
  show _ = "fn"

instance Show a => Show (V a) where
  show VRoot = "[root]"
  show (VConst a) = show a
  show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ (show vfb) ++ ")"
  show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ (show va) ++ ")"
  show (VSeal va) = "(seal " ++ (show va) ++ ")"

class Show w => History h w where
  mkHistory :: w -> h w
  getRoot :: h w -> V w
  newGeneration :: h w -> w -> h w
  addListener :: h w -> Listener -> h w
  runListeners :: h w -> IO ()
  r :: h w -> V a -> a
  wr :: h w -> V a -> a -> Write

sinfulCast :: (Typeable a, Typeable b) => a -> b
sinfulCast = fromJust . fromDynamic . toDyn

-- Stored in reverse order
data SimpleHistory w = SimpleHistory [w] [Listener]

instance Show w => Show (SimpleHistory w) where
  show (SimpleHistory ws _) = show ws

instance (Typeable w, Show w) => History SimpleHistory w where
  mkHistory w = SimpleHistory [w] []

  getRoot (SimpleHistory (w:_) _) = VRoot

  newGeneration (SimpleHistory ws ls) w = SimpleHistory (w:ws) ls

  addListener (SimpleHistory ws listeners) listener =
    SimpleHistory ws (listener:listeners)

  runListeners h@(SimpleHistory _ ls) = mapM_ runListener ls
    where runListener (Listener va action) = do
            let a = r h va
            action a

  -- r :: W -> V a -> a
  r (SimpleHistory (w:_) _) VRoot = sinfulCast w
  r _ (VConst x) = x
  -- TODO not crazy about constructing receivers here
  r h (VApp vfbfa vb) = r h (VSeal (VPartialApp vfbfa vb))
  -- r w (VApp vf va) = b
  --   where f = r w vf
  --         a = r w va
  --         -- rb = R b (Receiver $ \b' -> Write [Write1 b'])
  --         ra = R a (Receiver "r VApp" $ \a' -> Write [Write1 va a'])
  --         rb = f ra
  --         b = case rb of R b _ -> b
  r h (VSeal vra) = a
    where ra = r h vra
          a = case ra of R a _ -> a
  r h (VPartialApp vf va) = paf
    where f = r h vf
          a = r h va
          ra = R a (Receiver "r VPartialApp" $ \a' -> Write [Write1 va a'])
          paf = f ra

  -- wr :: W -> V a -> a -> Write
  -- wr w VRoot _ = undefined "Can't write to root"
  wr h (VConst _) _ = undefined "Can't write to a const"
  wr h (VApp vfbfa vb) b = wr h (VSeal (VPartialApp vfbfa vb)) b
    --where -- write = Write [Write1 vb b']
    --      write = reca a
    --      rbra = r w vfbfa
    --      -- ra = rbra rb
    --      R _ (Receiver reca) = rbra rb
    --      rb = R b (Receiver $ \b' -> Write [Write1 vb b'])
    --      b = r w vb
    --      --b' = undefined
  -- Good gravy why is this not needed?
  wr h (VPartialApp vf va) _ = error "Why is this error not happening"
  wr h (VSeal vra) a = write
    where write = {-eeesp ("REC wr2", s) $-} reca a
          R _ (Receiver s reca) = ra
          ra = r h vra

-- Monad!
type TMI h w a = (History h w) => StateT (h w) IO a

tmiRun :: (Typeable w, History h w) => h w -> TMI h w a -> IO (a, h w)
tmiRun history action = do
  runStateT action history

propagateFully :: (Typeable w, History h w) => h w -> Write -> w
propagateFully = propagateOneAtATime

-- This only works for a single assignment that always propagates to a single
-- assignment
propagateOneAtATime :: (Typeable w, History h w) => h w -> Write -> w
propagateOneAtATime h (Write [Write1 VRoot w]) = sinfulCast w
propagateOneAtATime h (Write [Write1 va a]) =
  let write' = wr h va a
   in propagateOneAtATime h write'

  -- wr :: h w -> V a -> a -> Write

infix 4 <---
(<---) :: (Typeable w, Show a) => V a -> V a -> TMI h w ()
vlvalue <--- vrvalue = do
  history <- get
  let rvalue = r history vrvalue
      write = Write [Write1 vlvalue rvalue]
      w' = propagateFully history write
      history' = newGeneration history w'
  put history'
  liftIO $ runListeners history'
  return ()

data Listener = forall a. Listener (V a) (a -> IO ())

listen :: Typeable a => V a -> (a -> IO ()) -> TMI h w ()
listen v action = do
  history <- get
  let listener = Listener v action
      history' = addListener history listener
  put history'
