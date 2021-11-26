{-# Language GADTs #-}

module Ty
( W(..)
, V(..)
, Bi(..)
, R(..)
, Write(..) ) where

import Type.Reflection (Typeable)

data W = W { aa :: Int, bb :: Int } deriving (Read, Show)

data Write = forall a. Write (V a) a | Writes [Write]

data R a = R (a -> Write)

data Bi f r where
  Bi :: (Typeable f, Typeable r) => V f -> V r -> Bi f r
  BiApp :: (Typeable a, Typeable b, Typeable c) => Bi (a -> b) (a -> R a -> c) -> V a -> Bi b c

data V a where
  VRoot :: V W
  VNice :: (Eq a, Show a, Read a, Typeable a) => a -> V a
  VNamed :: String -> a -> V a
  VBiSeal :: Bi a (R a) -> V a
