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

instance Eq (Bi f r) where
  Bi qf qr == Bi qf' qr' = qf == qf' && qr == qr'
  -- TODO: use dynamic stuff to do this?
  BiApp bi q == BiApp bi' q' = bi `biAppEq` bi' && q `qeq` q'
  _ == _ = False

biAppEq :: (Typeable a1, Typeable a2) => a1 -> a2 -> Bool
biAppEq bi bi' = biAppEqD (toDyn bi) (toDyn bi')
biAppEqD :: Dynamic -> Dynamic -> Bool
biAppEqD (Dynamic bit bi) (Dynamic bit' bi')
  | Just HRefl <- bit `eqTypeRep` bit'
  , App (App bitt x) y <- bit
  , Just HRefl <- bitt `eqTypeRep` (typeRep @Bi)
  = bi == bi'
biAppEqD _ _ = False
qeq q q' = qeqD (toDyn q) (toDyn q')
qeqD :: Dynamic -> Dynamic -> Bool
qeqD (Dynamic qt q) (Dynamic qt' q')
  | Just HRefl <- qt `eqTypeRep` qt'
  , App qtt x <- qt
  , Just HRefl <- qtt `eqTypeRep` (typeRep @Q)
  = q == q'
qeqD _ _ = False

-- BiApp :: Typeable a => Bi (a -> b) (a -> R a -> c) -> Q a -> Bi b c

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
