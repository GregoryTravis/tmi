--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.State hiding (lift)
import Data.Containers.ListUtils
import Data.Dynamic
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
--import GHC.Generics hiding (V1)

import Hash
import Util

data W = W { anInt :: Int, aString :: String }
  deriving (Eq, Show, Typeable)
instance Nice W
instance Nice Int
instance Nice String
instance (Nice a, Nice b) => Nice (a, b)

-- Contains a hash or fake hash
data Key = Key String
  deriving (Show, Eq, Ord)

class Keyable a where
  toKey :: a -> Key

-- compositeKey :: Keyable a => [a] -> Key
-- compositeKey keyables = Key $ hash $ concat $ map (\(Key s) -> s) $ map toKey keyables
compositeKey :: [Key] -> Key
compositeKey keys = Key $ hash $ concat $ map (\(Key s) -> s) keys

-- Values amenable to TMI.
-- Eq, Show in here for development, should remove.
class (Eq a, Show a, Typeable a) => Nice a

data Evaluator = Evaluator (V W) Dynamic
  deriving Show
evaluate :: Nice a => Evaluator -> V a -> a
evaluate evaluator@(Evaluator vw w) v | getKey v == getKey vw = fromJust $ fromDynamic w
                                      | otherwise = runForward v evaluator
data Write = forall a. (Show a, Nice a) => Write (V a) a
instance Show Write where
  show (Write v x) = "(Write " ++ show v ++ " " ++ show x ++ ")"

-- Need to support:
-- - give me a list of your arguments (as Keys)
-- - please pull values for your arguments from this map/fn, compute your output, and return it
-- - please pull values for your arguments from this map/fn, take a new output to compute new values for your arguments, and return those wrapped as writes
data V a = forall args. V { getKey :: Key
                          , getArgKeys :: [Key]
                          , runForward :: Evaluator -> a
                          , runReverse :: Evaluator -> a -> [Write] }
instance Show (V a) where
  show (V {..}) = "(V " ++ show getKey ++ " " ++ show getArgKeys ++ ")"
instance Keyable (V a) where
  toKey v = getKey v
instance Eq (V a) where
  x == y = getKey x  == getKey y
instance Ord (V a) where
  compare x y = compare (getKey x) (getKey y)

data Wrapped = forall a. Wrapped a (a -> Key)

instance Eq Wrapped where
  (Wrapped x f) == (Wrapped x' f') = (f x) == (f' x')
instance Ord Wrapped where
  compare (Wrapped x f) (Wrapped x' f') = compare (f x) (f' x')

wrapV :: Nice a => V a -> Wrapped
wrapV v = Wrapped v getKey

-- This is the only way to make a V without applying an F to an existing one
makeRoot :: V W
makeRoot =
  -- Recursive use of this v
  let v = V { getKey = worldKey
            , getArgKeys = []
            , runForward = \cache -> evaluate cache v
            , runReverse = \_ _ -> [] }
   in v
worldKey :: Key
worldKey = Key (hash "")  -- md5 hash of empty string will probably not collide with any V

data F a b = F String (a -> b) (a -> b -> a)
instance Keyable (F a b) where
  toKey (F name _ _) = Key (hash name)

data F2 a b c = F2 String (a -> b -> c) (a -> b -> c -> (a, b))
instance Keyable (F2 a b c) where
  toKey (F2 name _ _) = Key (hash name)

lift :: (Nice a, Nice b) => F a b -> (V a -> V b)
lift f@(F _ for rev) va = V { getKey = compositeKey [toKey f, toKey va]
                              , getArgKeys = [getKey va]
                              , runForward
                              , runReverse }
  where runForward cache = let a = evaluate cache va
                            in for a
        runReverse cache b' = let a = evaluate cache va
                                  a' = rev a b'
                               in [Write va a']

lift2 :: (Nice a, Nice b) => F2 a b c -> (V a -> V b -> V c)
lift2 f@(F2 _ for rev) va vb = V { getKey = compositeKey [toKey f, toKey va, toKey vb]
                                 , getArgKeys = [getKey va, getKey vb]
                                 , runForward
                                 , runReverse }
  where runForward cache = let a = evaluate cache va
                               b = evaluate cache vb
                            in for a b
        runReverse cache c' = let a = evaluate cache va
                                  b = evaluate cache vb
                                  (a', b') = rev a b c'
                               in [Write va a', Write vb b']

_anInt :: V W -> V Int
_anInt = lift $ F "anInt" anInt anInt_r
  where anInt_r w i = w { anInt = i }

_aString :: V W -> V String
_aString = lift $ F "aString" aString aString_r
  where aString_r w s = w { aString = s }

incer :: F Int Int
incer = F "incer" (+1) (\_ x -> x-1)

nextInt = lift incer

bother :: V Int -> V String -> V (Int, String)
bother = lift2 $ F2 "bother" (,) (\_ _ (i, s) -> (i, s))  -- Yeah I wanted to write it out

data History = History [W]
  deriving Show

initHistory :: W -> History
initHistory w = History [w]

updateHistory :: History -> [Write] -> History
--updateHistory _ ws | trace ("updateHistory", ws) = undefined
updateHistory (History []) _ = error "updateHistory: empty history"
updateHistory (History ws) writes = History (ws ++ [w])
  where w = checkAndGetW $ propagateWrites evaluator writes
        evaluator = Evaluator makeRoot (toDyn (last ws))

-- Check that there is only one disinct write for each node, and that there is
-- one such right for the root, and return that.
-- Even more restrictive: there should only be one write for each node.
checkAndGetW :: [Write] -> W
checkAndGetW writes = assertM "checkAndGetW" ok w
  where ok = length keys == length uniqueKeys
        keys :: [Key]
        keys = map get writes
        uniqueKeys :: [Key]  -- Not sure why I had to break this out
        uniqueKeys = nubOrd keys
        get (Write v x) = getKey v
        w :: W
        w = case filter isW writes of [Write vx w] -> (fromJust . fromDynamic . toDyn) w
        isW (Write v x) = getKey v == getKey makeRoot

propagateWrites :: Evaluator -> [Write] -> [Write]
--propagateWrites _ ws | trace ("propagateWrites", ws) = undefined
propagateWrites cache [] = []
propagateWrites cache (w:ws) = w : ws'
  where ws' = propagateWrites cache (propagateWrite cache w ++ ws)

propagateWrite :: Evaluator -> Write -> [Write]
--propagateWrite _ w | trace ("propagateWrite", w) = undefined
propagateWrite evaluator (Write v x) = runReverse v evaluator x

r :: Nice a => History -> V a -> a
r (History []) _ = error "r: empty history"
r (History ws) v = evaluate evaluator v
  where evaluator = Evaluator makeRoot (toDyn (last ws))

type TMI = StateT History IO ()

infix 4 <--
(<--) :: Nice a => V a -> a -> TMI
v <-- x = do
  history <- get
  let history' = updateHistory history [Write v x]
  put history'
  return ()

--rr :: Nice a => V a -> a

tmiRun :: W -> TMI -> IO History
tmiRun world action = do
  ((), history) <- runStateT action (History [world])
  return history

main = do
  noBuffering
  let world :: W
      world = W { anInt = 12, aString = "asdf" }
      h = initHistory world
      vw = makeRoot
      vai = _anInt vw
      vas = _aString vw
      vni = nextInt vai
      vboth = bother vni vas
      write = Write vni 130
      h' = updateHistory h [write]
  -- let writes = propagateWrites (case h of History vcs -> last vcs) [write]
  -- msp writes
  let hmm :: M.Map Wrapped Int
      hmm = M.insert (wrapV vai) 88 (M.insert (wrapV vboth) 99 M.empty)
  -- msp vw
  -- msp vai
  -- msp vni
  -- msp vboth
  -- msp write
  msp $ r h vw
  msp $ r h vai
  msp $ r h vas
  msp $ r h vni
  msp $ r h vboth
  msp $ r h' vw
  msp $ r h' vai
  msp $ r h' vas
  msp $ r h' vni
  msp $ r h' vboth
  h' <- tmiRun world $ do
          vni <-- 30
  msp $ r h' vni
  msp "hi"
