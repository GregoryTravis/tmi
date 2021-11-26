{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module V where

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

instance Show (V a) where
  show VRoot = "VRoot"
  show (VNice x) = "(VNice " ++ show x ++ ")"
  show (VNamed name _) = "(VNamed " ++ name ++ ")"
  show (VBiSeal bi) = "(VBiSeal " ++ show bi ++ ")"

instance Show (Bi f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"
