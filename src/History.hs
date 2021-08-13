module History
( History
, newGeneration
, latestState
, histlen
, mkHistory
, getRoot
) where

import V

-- Stored in reverse order
newtype History w = History [w]
-- TODO Foldable
histlen :: History w -> Int
histlen (History ws) = length ws

instance Show w => Show (History w) where
  show (History ws) = show ws

mkHistory :: w -> History w
mkHistory w = History [w]

latestState :: History w -> w
latestState (History []) = error "latestState:: empty"
latestState (History (w:_)) = w

getRoot :: History w -> V w
getRoot _ = VRoot

newGeneration :: History w -> w -> History w
newGeneration (History ws) w = History (w:ws)
