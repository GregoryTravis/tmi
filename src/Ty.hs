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

data Log w = Log
  { logCalls :: [V w (Call w)]
  , logEvents :: [Event]
  }
  -- deriving (Read, Show)

data Call w = forall a. (Read a, Show a) => Call (IO a) (a -> TMI w ())
-- vcallK :: V w (Call w) -> V w (a -> TMI w ())
-- vcallK = lift1 $ nuni "callK" (\(Call _ k) -> k)

data Event = RetVal String -- | Command
  deriving (Eq, Ord, Read, Show)

data Sys w = Sys { sysLog :: Log w }
  -- deriving (Read, Show)

data W app = W { wApp :: app, wSys :: Sys (W app) }
  -- deriving (Read, Show)

data TMI w a = TMI a
  deriving (Eq, Ord, Read, Show)
