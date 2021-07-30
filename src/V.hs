{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V where

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

data V a where
  VRoot :: V a
  VConst :: (Show a) => String -> a -> V a
  VCheckConst :: (Show a, Eq a) => String -> a -> V a
  VPartialApp :: (Show a) => V (R a -> rest) -> V a -> V rest
  VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
  VApp :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a) => V (R a) -> V a
  -- VUnSeal :: (Show a) => V a -> V (R a)

-- more succinct
k :: (Show a) => String -> a -> V a
k = VConst
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
