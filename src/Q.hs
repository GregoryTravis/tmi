{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module Q
( W(..)
, Q(..)
, Bi(..)
, R(..)
, Write(..) ) where

import Type.Reflection

data W = W { aa :: Int, bb :: Int } deriving (Read, Show)

data Write = forall a. Write (Q a) a | Writes [Write]
emptyWrite :: Write
emptyWrite = Writes []

instance Semigroup Write where
  w <> w' = Writes [w, w']

instance Show Write where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (Writes ws) = show ws
data R a = R (a -> Write)

data Bi f r where
  Bi :: Q f -> Q r -> Bi f r
  BiApp :: Bi (a -> b) (a -> R a -> c) -> Q a -> Bi b c

data Q a where
  QRoot :: Q W
  QNice :: (Show a, Read a, Typeable a) => a -> Q a
  QNamed :: String -> a -> Q a
  QBiSeal :: Bi a (R a) -> Q a

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QBiSeal bi) = "(QBiSeal " ++ show bi ++ ")"

instance Show (Bi f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"
