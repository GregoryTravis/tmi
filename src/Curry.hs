{-# LANGUAGE ExistentialQuantification, GADTs, RecordWildCards, StandaloneDeriving,
             TypeApplications, TypeFamilies #-}

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
, r
, wr
) where

import Control.Monad.Cont

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
  VRoot :: V W
  VConst :: Show a => a -> V a
  VPartialApp :: Show a => V (R a -> rest) -> V a -> V rest
  VApp :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a) => V (R a) -> V a

-- more succinct
k :: Show a => a -> V a
k = VConst
infixl 4 <**>
(<**>) :: Show a => V (R a -> rest) -> V a -> V rest
(<**>) = VPartialApp
infixl 4 <$$>
(<$$>) :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
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

r :: W -> V a -> a
r w VRoot = w
r _ (VConst x) = x
-- TODO not crazy about constructing receivers here
r w (VApp vfbfa vb) = r w (VSeal (VPartialApp vfbfa vb))
-- r w (VApp vf va) = b
--   where f = r w vf
--         a = r w va
--         -- rb = R b (Receiver $ \b' -> Write [Write1 b'])
--         ra = R a (Receiver "r VApp" $ \a' -> Write [Write1 va a'])
--         rb = f ra
--         b = case rb of R b _ -> b
r w (VSeal vra) = a
  where ra = r w vra
        a = case ra of R a _ -> a
r w (VPartialApp vf va) = paf
  where f = r w vf
        a = r w va
        ra = R a (Receiver "r VPartialApp" $ \a' -> Write [Write1 va a'])
        paf = f ra

wr :: W -> V a -> a -> Write
wr w VRoot _ = undefined "Can't write to root"
wr w (VConst _) _ = undefined "Can't write to a const"
wr w (VApp vfbfa vb) b = wr w (VSeal (VPartialApp vfbfa vb)) b
  --where -- write = Write [Write1 vb b']
  --      write = reca a
  --      rbra = r w vfbfa
  --      -- ra = rbra rb
  --      R _ (Receiver reca) = rbra rb
  --      rb = R b (Receiver $ \b' -> Write [Write1 vb b'])
  --      b = r w vb
  --      --b' = undefined
-- Good gravy why is this not needed?
wr w (VPartialApp vf va) _ = error "Why is this error not happening"
wr w (VSeal vra) a = write
  where write = {-eeesp ("REC wr2", s) $-} reca a
        R _ (Receiver s reca) = ra
        ra = r w vra

--class History 

{-
class History h w where
  mkHistory :: w -> h w
  addListener :: h w -> Listener -> h w
  write :: Nice w => h w -> [Write] -> IO (h w)
  -- TODO debug onlyl
  readV :: (Show a, Nice w, Nice a) => h w -> V a -> IO a
  runListeners :: Nice w => h w -> IO ()

data Listener = forall a. Nice a => Listener
  { v :: V a
  , action :: a -> IO ()
  , runReader :: Reader -> IO ()
  , getDv :: DV }

-- Monad!
type TMI h w a = (Nice w, History h w) => StateT (h w) IO a

infix 4 <--
(<--) :: Nice a => V a -> V a -> TMI h w ()
vlvalue <-- vrvalue = do
  history <- get
  rvalue <- rd vrvalue
  let wr = Write (dyv vlvalue) (dy rvalue)
  history' <- liftIO $ write history [wr]
  put history'
  return ()

mkListener :: Nice a => V a -> (a -> IO ()) -> Listener
mkListener v action = Listener {..}
  where getDv = dyv v
        runReader :: Reader -> IO ()
        runReader reader = do
          a <- unReader reader v
          action a

listen :: Nice a => V a -> (a -> IO ()) -> TMI h w ()
listen v action = do
  history <- get
  let listener = mkListener v action
      history' = addListener history listener
  put history'

dump :: TMI h w ()
dump = do
  history <- get
  liftIO $ runListeners history

tmiRun :: (Nice w, History h w) => w -> TMI h w a -> IO (a, h w)
tmiRun w action = do
  let history = mkHistory w
  runStateT action history
-}
