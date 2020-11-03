{-# LANGUAGE MultiParamTypeClasses #-}

module History
(
) where

import Tmi

class HistoryT h w where
  -- TODO this has to be a homogeneous collection of listenees, not (V a), and not Key
  init :: w -> [V a] -> h w
  read :: h w -> V a -> a
  write :: h w -> V a -> a -> Write
