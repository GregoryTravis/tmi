{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module V where

import Ty

emptyWrite :: Write w
emptyWrite = Writes []

instance Semigroup (Write w) where
  w <> w' = Writes [w, w']

write :: R w a -> a -> Write w
write (R rec) x = rec x

k :: Show a => a -> V w a
k a = VNamed "" a

mkR :: (a -> Write w) -> R w a
mkR = R

bi :: V w f -> V w r -> Bi w f r
bi = Ty.Bi
vbi :: String -> f -> r -> Bi w f r
vbi s f r = (bi (VNamed s f) (VNamed (s ++ "_") r))

-- Turned a named forward into a Bi
uni :: V w (a -> b) -> Bi w (a -> b) (a -> R w a -> c)
uni vf = Bi vf nope
nope = VNamed "nope" (error "nope")

nuni :: String -> (a -> b) -> Bi w (a -> b) (a -> R w a -> c)
nuni name f = uni (VNamed name f)

instance Show (Write w) where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (VWrite qa qa') = "(VWrite " ++ show qa ++ " " ++ show qa' ++ ")"
  show (Writes ws) = show ws

instance Show (V w a) where
  show VDummy = "VDummy"
  show VRoot = "VRoot"
  show (VNice x) = "(VNice " ++ show x ++ ")"
  show (VNamed name _) = "(VNamed " ++ name ++ ")"
  show (VBiSeal bi) = "(VBiSeal " ++ show bi ++ ")"

instance Show (Bi w f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"
