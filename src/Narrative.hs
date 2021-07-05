{-# LANGUAGE
    ExistentialQuantification
  #-}

module Narrative
( narrativeMain
) where

import Control.Monad
import Control.Monad.State.Lazy
-- import Data.Dynamic
import Data.Maybe
import Data.Time.Clock.System
import Data.Time.Clock
import qualified Data.Map.Strict as M
import System.Directory (getDirectoryContents)

-- import Dyno
import Util

type Serial = Int
type Memo = M.Map Serial String
data Blah = Blah Serial Memo deriving Show
emptyBlah :: Blah
emptyBlah = Blah 0 M.empty
type Narrative a = StateT Blah IO a

foo :: (Read a, Show a, Eq a) => IO a -> Narrative a
foo action = do
  Blah i memo <- get
  case memo M.!? i of
    Just dyn -> do
      put $ Blah (i + 1) memo
      liftIO $ msp "hit"
      report
      save
      return $ read dyn
    Nothing -> do
      liftIO $ msp "miss"
      a <- liftIO action
      let memo' = M.insert i da memo
          da = show a
      put $ Blah (i + 1) memo'
      report
      save
      return a

timey :: IO UTCTime
timey = do
  t <- getSystemTime
  return $  systemToUTCTime t

report :: Narrative ()
report = do
  blah <- get
  liftIO $ msp ("Blah", blah)

save :: Narrative ()
save = do
  Blah _ memo <- get
  let memo' = show memo
  liftIO $ writeFile "save.txt" memo'

bar :: Narrative ()
bar = do
  t <- foo timey
  liftIO $ msp t
  t' <- foo timey
  liftIO $ msp t'
  files <- foo $ getDirectoryContents "."
  liftIO $ msp files
  if "out" `elem` files
    then foo $ msp "found it"
    else foo $ msp "didn't find it"
  return ()

--loadAndRun :: Narrative a -> a
loadAndRun action = do
  memo <- liftIO $ readFile "save.txt"
  let memo' = read memo
      blah = Blah 0 memo'
  runStateT action blah

narrativeMain = do
  -- ((), Blah _ memo) <- runStateT bar emptyBlah
  -- ((), blah') <- runStateT bar (Blah 0 memo)
  ((), blah'') <- loadAndRun bar
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
