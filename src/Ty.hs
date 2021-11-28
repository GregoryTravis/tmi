{-# Language GADTs #-}

module Ty where

import Control.Monad.State.Lazy hiding (execState)
import Type.Reflection (Typeable)

data Write w = forall a. Write (V w a) a | Writes [Write w]

data R w a = R (a -> Write w)

data Bi w f r where
  Bi :: (Typeable f, Typeable r, Typeable w) => V w f -> V w r -> Bi w f r
  BiApp :: (Typeable a, Typeable b, Typeable c, Typeable w) => Bi w (a -> b) (a -> R w a -> c) -> V w a -> Bi w b c

data V w a where
  VRoot :: V w w
  VNice :: (Eq a, Show a, Read a, Typeable a) => a -> V w a
  VNamed :: String -> a -> V w a
  VBiSeal :: Bi w a (R w a) -> V w a

data S w = S
  { initSate :: w
  , steps :: [Step w]
  , retvals :: [Retval] }

data Step w = forall a. (Typeable w, Typeable a, Read a, Show a) => Step (IO a) (a -> TMI w ())

data Retval = Retval String
  deriving (Eq, Show, Read)
mkRetval :: Show a => a -> Retval
mkRetval = Retval . show

type TMI w a = StateT (S w) IO a
