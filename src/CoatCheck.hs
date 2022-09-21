{-# LANGUAGE RecordWildCards, StandaloneDeriving #-}

module CoatCheck
( CoatCheck
, Tag
, empty
, null
, check
, retrieve
, size ) where

import Prelude hiding (map, null)
import Data.Map.Strict hiding (empty, map, null, size)
import qualified Data.Map.Strict as M

data Tag = Tag Int
  deriving (Eq, Ord, Read, Show)

data CoatCheck a = CoatCheck
  { serial :: Int
  , map :: Map Tag a
  }

deriving instance Show a => Show (CoatCheck a)
deriving instance Read a => Read (CoatCheck a)

empty :: CoatCheck a
empty = CoatCheck { serial = 0, map = M.empty }

null :: CoatCheck a -> Bool
null (CoatCheck {..}) = M.null map

check :: CoatCheck a -> a -> (Tag, CoatCheck a)
check (CoatCheck {..}) a =
  let tag = Tag serial
      serial' = serial + 1
      map' = insert tag a map
      cc' = CoatCheck { serial = serial', map = map' }
   in (tag, cc')

retrieve :: CoatCheck a -> Tag -> Maybe (a, CoatCheck a)
retrieve cc tag =
  case map cc !? tag
    of Nothing -> Nothing
       Just a -> Just (a, cc')
  where map' = delete tag (map cc)
        cc' = cc { map = map' }

size :: CoatCheck a -> Int
size cc = M.size (map cc)
