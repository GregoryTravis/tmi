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
import qualified Debug.Trace as TR

import Hash
import Util

data W = W { anInt :: Int, aString :: String }
  deriving (Show, Typeable)
instance Nice W
instance Nice Int
instance Nice String

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
-- Show in here for development, should remove
class (Show a, Typeable a) => Nice a

data ValueCache = ValueCache (M.Map Key Dynamic)
readCache :: Nice a => ValueCache -> V a -> a
readCache (ValueCache m) v = fromJust $ fromDynamic (fromJust (m M.!? (getKey v)))
writeCache :: Nice a => ValueCache -> V a -> a -> ValueCache
writeCache (ValueCache m) v x = ValueCache m'
  where m' = M.insert (getKey v) (toDyn x) m
emptyValueCache = ValueCache M.empty

data Write = forall a. Show a => Write (V a) a
instance Show Write where
  show (Write v x) = "(Write " ++ show v ++ " " ++ show x ++ ")"
data DW = DW [Write]
  deriving Show

-- Need to support:
-- - give me a list of your arguments (as Keys)
-- - please pull values for your arguments from this map/fn, compute your output, and return it
-- - please pull values for your arguments from this map/fn, take a new output to compute new values for your arguments, and return those wrapped as writes
data V a = forall args. V { getKey :: Key
                          , getArgKeys :: [Key]
                          , runForward :: ValueCache -> a
                          , runReverse :: ValueCache -> a -> DW }
instance Show (V a) where
  show (V {..}) = "(V " ++ show getKey ++ " " ++ show getArgKeys ++ ")"
instance Keyable (V a) where
  toKey v = getKey v

-- This is the only way to make a V without applying an F to an existing one
makeRoot :: W -> V W
makeRoot w =
  -- Recursive use of this v
  let v = V { getKey = worldKey
            , getArgKeys = []
            , runForward = \cache -> readCache cache v
            , runReverse = \cache w -> DW [Write v w] }
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
  where runForward cache = let a = readCache cache va
                            in for a
        runReverse cache b' = let a = readCache cache va
                                  a' = rev a b'
                               in DW [Write va a']

lift2 :: (Nice a, Nice b) => F2 a b c -> (V a -> V b -> V c)
lift2 f@(F2 _ for rev) va vb = V { getKey = compositeKey [toKey f, toKey va, toKey vb]
                                 , getArgKeys = [getKey va, getKey vb]
                                 , runForward
                                 , runReverse }
  where runForward cache = let a = readCache cache va
                               b = readCache cache vb
                            in for a b
        runReverse cache c' = let a = readCache cache va
                                  b = readCache cache vb
                                  (a', b') = rev a b c'
                               in DW [Write va a', Write vb b']

_anInt :: V W -> V Int
_anInt = lift $ F "anInt" anInt anInt_r
  where anInt_r w i = w { anInt = i }

incer :: F Int Int
incer = F "incer" (+1) (\_ x -> x-1)

nextInt = lift incer

main = do
  let world :: W
      world = W { anInt = 12, aString = "asdf" }
  let vw = makeRoot world
      cache = writeCache emptyValueCache vw world
      vai = _anInt vw
      vni = nextInt vai
  msp $ runForward vai cache
  msp $ runReverse vai cache 120
  let cache' = writeCache cache vai (runForward vai cache)
  msp $ runForward vni cache'
  msp $ runReverse vni cache' 130
  msp "hi"
