{-# Language GADTs #-}

module Ty
( W(..)
, Q(..)
, Bi(..)
, R(..)
, Write(..) ) where

import Type.Reflection (Typeable)

data W = W { aa :: Int, bb :: Int } deriving (Read, Show)

data Write = forall a. Write (Q a) a | Writes [Write]

data R a = R (a -> Write)

data Bi f r where
  Bi :: Q f -> Q r -> Bi f r
  BiApp :: Bi (a -> b) (a -> R a -> c) -> Q a -> Bi b c

data Q a where
  QRoot :: Q W
  QNice :: (Show a, Read a, Typeable a) => a -> Q a
  QNamed :: String -> a -> Q a
  QBiSeal :: Bi a (R a) -> Q a
