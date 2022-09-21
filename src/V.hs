{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module V
( write
, k
, mkR
, bi
, vbi
, nope
, nuni
, vSize
) where

import Ty

import Data.Dynamic (Typeable)

emptyWrite :: Write w
emptyWrite = Writes []

instance Semigroup (Write w) where
  w <> w' = Writes [w, w']

write :: R w a -> a -> Write w
write (R rec) x = rec x

k :: (Typeable a, Eq a, Show a, Read a) => a -> V w a
k a = VNice a

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

deref :: V w (V w a) -> V w a
deref = VDeref

vSize :: V w a -> Int
vSize VRoot = 1
vSize (VNice _) = 1
vSize (VNamed _ _) = 1
vSize (VBiSeal bi) = bSize bi
vSize (VDeref vva) = error "vSize VDeref"

bSize :: Bi w f r -> Int
bSize (Bi _ _) = 1
bSize (BiApp bi' va) = bSize bi' + vSize va
