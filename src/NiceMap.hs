{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, RecordWildCards, StandaloneDeriving #-}

module NiceMap
( NiceMap
, Tag
, empty
, null
, insert
, lookup
, delete ) where

import Prelude hiding (null, lookup, insert, delete)
import qualified Data.Map.Strict as M
import Unsafe.Coerce

data Tag = Tag Int
  deriving (Eq, Ord, Read, Show)

data NiceMap = NiceMap
  { serial :: Int
  , mapp :: M.Map Tag String
  }
  deriving (Read, Show)

empty :: NiceMap
empty = NiceMap { serial = 0, mapp = M.empty }

null :: NiceMap -> Bool
null (NiceMap {..}) = M.null mapp

insert :: (Read a, Show a) => a -> NiceMap -> (Tag, NiceMap)
insert a (NiceMap {..}) =
  let tag = Tag serial
      serial' = serial + 1
      mapp' = M.insert tag (show a) mapp
      cc' = NiceMap { serial = serial', mapp = mapp' }
   in (tag, cc')

lookup :: (Read a, Show a) => Tag -> NiceMap -> Maybe a
lookup tag cc = read <$> M.lookup tag (mapp cc)

delete :: Tag -> NiceMap -> NiceMap
delete tag cc = cc { mapp = M.delete tag (mapp cc) }
