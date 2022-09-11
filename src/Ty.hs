{-# Language GADTs, NamedFieldPuns #-}

module Ty where

import Control.Monad.State.Lazy hiding (execState)
import Data.Dynamic (Typeable)

import CoatCheck
import NiceMap

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

data H w = H
  { calls :: CoatCheck (V w (CPS w ()))
  , events :: [Event]
  , generations :: [w] -- newest first
  , todo :: [V w (CPS w ())]
  , store :: NiceMap
  }

data Event = RetVal String -- | Command
  deriving (Eq, Ord, Read, Show)

data Step w a where
  Ext :: (Read a, Show a) => IO a -> Step w a
  Ret :: a -> Step w a
  -- WriteStep :: V w a -> a -> Step ()
  WriteStep :: Write w -> Step w ()

data TMI w a where
  Step :: Step w a -> TMI w a
  Bind :: TMI w a -> (a -> TMI w b) -> TMI w b
  -- Par :: TMI w a -> TMI w b -> ((a, b) -> TMI w c) -> TMI w c
  -- deriving (Eq, Ord, Read, Show)

-- TMI in CPS form
-- TODO is a always ()?
data CPS w a where
  KBind :: Step w a -> (a -> CPS w b) -> CPS w b
  -- TODO shouln'd this be CPS w ()?
  Done :: CPS w a
