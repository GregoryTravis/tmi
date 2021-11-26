{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module Q where

import Data.Dynamic
import Type.Reflection

import Ty

emptyWrite :: Write
emptyWrite = Writes []

instance Semigroup Write where
  w <> w' = Writes [w, w']

instance Show Write where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (Writes ws) = show ws

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QBiSeal bi) = "(QBiSeal " ++ show bi ++ ")"

instance Show (Bi f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"
