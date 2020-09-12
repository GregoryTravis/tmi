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

data ValueCache = ValueCache (M.Map Key Dynamic)
  deriving Show
readCache :: Nice a => ValueCache -> V a -> a
readCache (ValueCache m) v = fromJust $ fromDynamic (fromJust (m M.!? (getKey v)))
writeCache :: Nice a => ValueCache -> V a -> a -> ValueCache
writeCache (ValueCache m) v x = ValueCache m'
  where m' = M.insert (getKey v) (toDyn x) m
writeCacheWrite :: ValueCache -> Write -> ValueCache
writeCacheWrite vc (Write v x) = writeCache vc v x
emptyValueCache = ValueCache M.empty
valueCacheUnion :: ValueCache -> ValueCache -> ValueCache
valueCacheUnion (ValueCache m0) (ValueCache m1) = ValueCache $ m0 `M.union` m1

-- Does not check that the set of writes is ok
updateValueCache :: ValueCache -> [Write] -> ValueCache
updateValueCache vc writes = vc `valueCacheUnion` (writesToValueCache writes)

writesToValueCache :: [Write] -> ValueCache
writesToValueCache writes = foldl writeCacheWrite emptyValueCache writes

data Write = forall a. (Show a, Nice a) => Write (V a) a
instance Show Write where
  show (Write v x) = "(Write " ++ show v ++ " " ++ show x ++ ")"

-- Need to support:
-- - give me a list of your arguments (as Keys)
-- - please pull values for your arguments from this map/fn, compute your output, and return it
-- - please pull values for your arguments from this map/fn, take a new output to compute new values for your arguments, and return those wrapped as writes
data V a = forall args. V { getKey :: Key
                          , runForward :: ValueCache -> a
                          , runReverse :: ValueCache -> a -> [Write] }
instance Show (V a) where
  show (V {..}) = "(V " ++ show getKey ++ ")"
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
            , runForward = \cache -> readCache cache v
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
                              , runForward
                              , runReverse }
  where runForward cache = let a = readCache cache va
                            in for a
        runReverse cache b' = let a = readCache cache va
                                  a' = rev a b'
                               in [Write va a']

lift2 :: (Nice a, Nice b) => F2 a b c -> (V a -> V b -> V c)
lift2 f@(F2 _ for rev) va vb = V { getKey = compositeKey [toKey f, toKey va, toKey vb]
                                 , runForward
                                 , runReverse }
  where runForward cache = let a = readCache cache va
                               b = readCache cache vb
                            in for a b
        runReverse cache c' = let a = readCache cache va
                                  b = readCache cache vb
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

data History = History [ValueCache]
  deriving Show

initHistory :: W -> History
initHistory w = updateHistory fauxEmptyHistory [Write makeRoot w]
  where -- This is 'faux' because the initial value cache has nothing in it,
        -- which is fine because it doesn't need to be read
        fauxEmptyHistory = History [emptyValueCache]

updateHistory :: History -> [Write] -> History
--updateHistory _ ws | trace ("updateHistory", ws) = undefined
updateHistory (History []) _ = error "updateHistory: empty history"
updateHistory (History vcs) writes = History (vcs ++ [vc'])
  where allWrites = assertWritesOk $ propagateWrites mostRecentValueCache writes
        mostRecentValueCache = last vcs
        vc' = updateValueCache mostRecentValueCache allWrites

assertWritesOk :: [Write] -> [Write]
assertWritesOk writes = assertM "checkWrites" (checkWrites writes) writes

-- TODO diagnostics, like # of repeated but same writes
checkWrites :: [Write] -> Bool
checkWrites ws = True
 
propagateWrites :: ValueCache -> [Write] -> [Write]
--propagateWrites _ ws | trace ("propagateWrites", ws) = undefined
propagateWrites cache [] = []
propagateWrites cache (w:ws) = w : ws'
  where ws' = propagateWrites cache (propagateWrite cache w ++ ws)

propagateWrite :: ValueCache -> Write -> [Write]
--propagateWrite _ w | trace ("propagateWrite", w) = undefined
propagateWrite vc (Write v x) = runReverse v vc x

r :: Nice a => History -> V a -> a
r (History []) _ = error "r: empty history"
r (History vcs) v = readCache (last vcs) v

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
  msp vw
  msp vai
  msp vni
  msp vboth
  -- msp write
  -- msp h'
  -- msp $ r h vw
  msp "hi"
