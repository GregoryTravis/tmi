{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

-- data Write1 = forall a. Write1 a
data Write1 = forall a. Show a => Write1 (V a) a
deriving instance Show Write1
newtype Write = Write [Write1] deriving Show
emptyWrite :: Write
emptyWrite = Write []
instance Semigroup Write where
  Write ws <> Write ws' = Write $ ws ++ ws'
data Receiver a = Receiver String (a -> Write)
instance Show (Receiver a) where
  show (Receiver s _) = "REC Receiver " ++ s
-- data Receiver a = Receiver (V a)
data R a = R a (Receiver a)
  deriving Show
infix 1 <--
(<--) :: Receiver a -> a -> Write
Receiver s r <-- x = {-eeesp ("REC <-- call", s) $-} r x
-- Receiver va <-- x = Write [Write1 va x]
-- Receiver va <-- x = Write [Write1 va x]

-- No; all you can get from these first two arguments is two Writes
-- whaa :: (Receiver a -> Write) -> (Receiver a -> Write) -> Write
-- whaa recRecA recRecB =
--   recRecA ra
--     where ra a = 

-- Combine trickly receivers for two functions into one
split :: (a -> b -> c) -> Receiver c -> (Receiver a -> Write) -> (Receiver b -> Write) -> Write
split f (Receiver _ rc) recA2w recB2w =
  let ra a =
        let rb b =
              let c = f a b
               in rc c
         in recB2w (Receiver "" rb)
   in recA2w (Receiver "" ra)
             

instance Contravariant Receiver where
  -- f >$< Receiver name a2w = Receiver name (a2w . f)
  contramap f (Receiver name a2w) = Receiver name (a2w . f)
-- Why is this necessary?
(>$<) :: Contravariant f => (a -> b) -> (f b -> f a)
(>$<) = contramap

instance Divisible Receiver where
  divide f rb rc = Receiver "divisible" ra
    where ra a = case f a of (b, c) -> (rb <-- b) <> (rc <-- c)
  conquer = Receiver "conquer" (const emptyWrite)

-- class Contravariant f => Divisible f where
--   --- | If one can handle split `a` into `(b, c)`, as well as handle `b`s and `c`s, then one can handle `a`s
--   divide  :: (a -> (b, c)) -> f b -> f c -> f a

--   -- | Conquer acts as an identity for combining @Divisible@ functors.
--   conquer :: f a

renameReceiver :: String -> Receiver a -> Receiver a
renameReceiver name (Receiver _ r) = Receiver name r

data V a where
  VRoot :: V a
  -- This is really VNamed
  VConst :: String -> a -> V a
  -- This is really VNiceConst
  VCheckConst :: (Show a, Read a, Eq a) => String -> a -> V a
  VPartialApp :: (Show a) => V (R a -> rest) -> V a -> V rest
  VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
  VApp :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a) => V (R a) -> V a
  -- VUnSeal :: (Show a) => V a -> V (R a)

vconst :: (Show a) => String -> a -> V a
vconst = VConst
vcheckconst :: (Show a, Read a, Eq a) => String -> a -> V a
vcheckconst = VCheckConst
vunapp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
vunapp = VUnPartialApp

infixl 4 <**>
(<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
(<**>) = VPartialApp
infixl 4 <$$>
(<$$>) :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
(<$$>) = VApp

-- TODO do not swat me
instance Show (a -> b) where
  show _ = "fn"

instance Show a => Show (V a) where
  show VRoot = "[root]"
  show (VConst s a) = "(VConst " ++ s ++ " " ++ show a ++ ")"
  -- TODO: Replace VConst with this, and add Eq to everything
  show (VCheckConst s a) = "(VCheckConst " ++ s ++ " " ++ show a ++ ")"
  -- show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ (show vfb) ++ ")"
  -- show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ (show va) ++ ")"
  show (VApp vfba vfb) = "(" ++ show vfba ++ " " ++ "arg" ++ ")"
  show (VPartialApp vf va) = "(VPartialApp " ++ show vf ++ " " ++ "arg" ++ ")"
  show (VUnPartialApp pa) = "(VUnPartialApp " ++ show pa ++ ")"
  show (VSeal va) = "(seal " ++ show va ++ ")"

-- Trying some persistence. VUnPartialApp is not handled.
data VRep = VRepRoot | VRepConst String | VRepCheckConst String | VRep1 String VRep | VRep2 String VRep VRep
  deriving (Read, Show)

vToVRep :: V a -> VRep
vToVRep VRoot = VRepRoot
-- vToVRep (VConst s x) = VRepConst (show (s, x))
vToVRep (VConst s x) = VRepConst s
vToVRep (VCheckConst s x) = VRepCheckConst (show (s, x))
vToVRep (VPartialApp vf va) = VRep2 "VPartialApp" (vToVRep vf) (vToVRep va)
vToVRep (VUnPartialApp vf) = undefined
vToVRep (VApp vf va) = VRep2 "VApp" (vToVRep vf) (vToVRep va)
vToVRep (VSeal va) = VRep1 "VSeal" (vToVRep va)

vRepToV :: VRep -> V a
vRepToV VRepRoot = VRoot
-- Can't say anything about Show for the subnodes
-- vRepToV (VRep2 "VPartialApp" vrf vra) = VPartialApp (vRepToV vrf) (vRepToV vra)
-- Requires Read on VConst
vRepToV (VRepConst s) =
  let x = reconstitute s
   in VConst s x
-- Needs Read
-- vRepToV (VRepCheckConst sx) =
--   let (s, x) = read sx
--    in VCheckConst s x

reconstitute :: String -> a
reconstitute = undefined
