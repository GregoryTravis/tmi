{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module V where

import Data.Dynamic
import Type.Reflection

import Ty

emptyWrite :: Write w
emptyWrite = Writes []

instance Semigroup (Write w) where
  w <> w' = Writes [w, w']

write :: R w a -> a -> Write w
write (R rec) x = rec x

mkR :: (a -> Write w) -> R w a
mkR = R

uni :: (Typeable a, Typeable b, Typeable w, Typeable c) => V w (a -> b) -> Bi w (a -> b) (a -> R w a -> c)
uni vf = Bi vf nope
  where nope = VNamed "nope" (error "nope")

instance Show (Write w) where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (Writes ws) = show ws

instance Show (V w a) where
  show VRoot = "VRoot"
  show (VNice x) = "(VNice " ++ show x ++ ")"
  show (VNamed name _) = "(VNamed " ++ name ++ ")"
  show (VBiSeal bi) = "(VBiSeal " ++ show bi ++ ")"

instance Show (Bi w f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"
