{-# LANGUAGE
    ExistentialQuantification
  #-}

module Narrative
( narrativeMain
) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Time.Clock.System (getSystemTime)
import qualified Data.Map.Strict as M
import System.Directory (getDirectoryContents)

import Dyno
import Util
type Serial = Int
type Memo = M.Map Serial Dyno
data Blah = Blah Serial Memo deriving Show
emptyBlah :: Blah
emptyBlah = Blah 0 M.empty
type Narrative a = StateT Blah IO a

foo :: (Show a, Eq a, Typeable a) => IO a -> Narrative a
foo action = do
  Blah i memo <- get
  case memo M.!? i of
    Just dyn -> do
      put $ Blah (i + 1) memo
      liftIO $ msp "hit"
      report
      return $ getit' dyn
    Nothing -> do
      liftIO $ msp "miss"
      a <- liftIO action
      let memo' = M.insert i da memo
          da = mkDyno a
      put $ Blah (i + 1) memo'
      report
      return a

report :: Narrative ()
report = do
  blah <- get
  liftIO $ msp ("Blah", blah)

bar :: Narrative ()
bar = do
  t <- foo getSystemTime
  liftIO $ msp t
  t' <- foo getSystemTime
  liftIO $ msp t'
  files <- foo $ getDirectoryContents "."
  liftIO $ msp files
  return ()

-- bar = do
--   t <- foo getSystemTime
--   msp ("ha", t)
--   files <- foo getDirectoryContents "."
--   msp ("ho", files)

narrativeMain = do
  ((), memo) <- runStateT bar emptyBlah
  msp "Narrative hi"

-- data Narrative = Narrative Serial Memo
-- data Narrative a = Narrative (IO a)

-- newtype NarrativeT m a = StateT { runStateT :: s -> m (a,s) }

-- type Narrative a = StateT Blah IO a

-- instance Monad Narrative where
-- -- (>>=) :: forall a b. m a -> (a -> m b) -> m b 
--   (Narrative a) >>= a2mb = 
--   -- pure and of either <*> or liftA2.
--   -- fmap

-- type Narrative a = StateT Blah 
