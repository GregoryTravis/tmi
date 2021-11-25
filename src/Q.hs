{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module Q where

import Type.Reflection

import Ty

emptyWrite :: Write
emptyWrite = Writes []

instance Semigroup Write where
  w <> w' = Writes [w, w']

instance Show Write where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (Writes ws) = show ws

instance Eq (Bi f r) where
  Bi qf qr == Bi qf' qr' = qf == qf' && qr == qr'
  -- TODO: use dynamic stuff to do this?
  -- BiApp bi q == BiApp bi' q' = bi == bi' && q == q'
  _ == _ = False

instance Eq (Q a) where
  QRoot == QRoot = True
  QNice x == QNice y = x == y
  QNamed name _ == QNamed name' _ = name == name'
  QBiSeal bi == QBiSeal bi' = bi == bi'
  _ == _ = False

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QBiSeal bi) = "(QBiSeal " ++ show bi ++ ")"

instance Show (Bi f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"
