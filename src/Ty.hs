{-# Language GADTs #-}

module Ty where

import Data.Dynamic (Typeable)

import Control.Monad.State.Lazy hiding (execState)

data Write w = forall a. Write (V w a) a | Writes [Write w]

data R w a = R (a -> Write w)

data Bi w f r where
  Bi :: V w f -> V w r -> Bi w f r
  BiApp :: Bi w (a -> b) (a -> R w a -> c) -> V w a -> Bi w b c

data V w a where
  VRoot :: V w w
  VNice :: (Typeable a, Eq a, Show a, Read a) => a -> V w a
  VNamed :: String -> a -> V w a
  VBiSeal :: Bi w a (R w a) -> V w a

data S w = S
  { initSate :: w
  , steps :: [Step w]
  , retvals :: [Retval] }

data Step w = forall a. Read a => Step (IO a) (a -> TMI w ())

data Retval = Retval String
  deriving (Eq, Show, Read)
mkRetval :: Show a => a -> Retval
mkRetval = Retval . show

type TMI w a = StateT (S w) IO a
