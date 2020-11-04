{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tmi
( V(..)
, F(..)
, F2(..)
, Key(..)
, W(..)
, Wrapped
, Write(..)
, lift
, lift2
, History(..)
) where

import Data.Dynamic

import Hash
import Util

data W = W { anInt :: Int, aString :: String }
  deriving (Eq, Read, Show, Typeable)
instance Nice W
instance Nice Int
instance Nice String
instance (Nice a, Nice b) => Nice (a, b)

data Write = forall a. (Show a, Nice a) => Write (V a) a
instance Show Write where
  show (Write v x) = "(Write " ++ show v ++ " " ++ show x ++ ")"

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

-- Need to support:
-- - give me a list of your arguments (as Keys)
-- - please pull values for your arguments from this map/fn, compute your output, and return it
-- - please pull values for your arguments from this map/fn, take a new output to compute new values for your arguments, and return those wrapped as writes
data V a = forall args. V { getKey :: Key
                          , getArgKeys :: [Key]
                          , runForward :: History W -> a
                          , runReverse :: History W -> a -> [Write] }
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
  where runForward history = let a = readV history va
                              in for a
        runReverse history b' = let a = readV history va
                                    a' = rev a b'
                                 in [Write va a']

lift2 :: (Nice a, Nice b) => F2 a b c -> (V a -> V b -> V c)
lift2 f@(F2 _ for rev) va vb = V { getKey = compositeKey [toKey f, toKey va, toKey vb]
                                 , getArgKeys = [getKey va, getKey vb]
                                 , runForward
                                 , runReverse }
  where runForward history = let a = readV history va
                                 b = readV history vb
                              in for a b
        runReverse history c' = let a = readV history va
                                    b = readV history vb
                                    (a', b') = rev a b c'
                                 in [Write va a', Write vb b']

data History w = History
  { readV :: forall a. V a -> a }
  -- , writeV :: forall a. V a -> a -> Write }
