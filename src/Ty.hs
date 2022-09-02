{-# Language GADTs, NamedFieldPuns #-}

module Ty where

import Control.Monad.State.Lazy hiding (execState)
import Data.Dynamic (Typeable)

data Write w = forall a. Write (V w a) a | forall a. VWrite (V w a) (V w a) | Writes [Write w]

data R w a = R (a -> Write w)

data Bi w f r where
  Bi :: V w f -> V w r -> Bi w f r
  BiApp :: Bi w (a -> b) (a -> R w a -> c) -> V w a -> Bi w b c

data V w a where
  -- VDummy :: V w w
  VRoot :: V w w
  VNice :: (Typeable a, Eq a, Show a, Read a) => a -> V w a
  VNamed :: String -> a -> V w a
  VBiSeal :: Bi w a (R w a) -> V w a
  VDeref :: V w (V w a) -> V w a

-- TODO is b every anything but ()?
data Call w b = forall a. Call (Step a) (a -> CPS w b)

data H w = H
  { calls :: [V w (Call w ())]
  , events :: [Event]
  , generations :: [w]
  , todo :: [V w (CPS w ())]
  }

data Event = RetVal String -- | Command
  deriving (Eq, Ord, Read, Show)

data Step a where
  Ext :: (Read a, Show a) => IO a -> Step a
  Ret :: a -> Step a

data TMI w a where
  Step :: Step a -> TMI w a
  Bind :: TMI w a -> (a -> TMI w b) -> TMI w b
  -- deriving (Eq, Ord, Read, Show)

-- TMI in CPS form
data CPS w a where
  KBind :: Step a -> (a -> CPS w b) -> CPS w b
  -- TODO shouln'd this be CPS w ()?
  Done :: CPS w a
