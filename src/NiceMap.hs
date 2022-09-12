{-# LANGUAGE ExistentialQuantification, NamedFieldPuns, RecordWildCards, StandaloneDeriving #-}

module NiceMap
( NiceMap
, Tag
, empty
, null
, alloc
, store
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

alloc :: NiceMap -> (Tag, NiceMap)
alloc nm@(NiceMap {..}) =
  let tag = Tag serial
      serial' = serial + 1
      cc' = nm { serial = serial' }
   in (tag, cc')

store :: (Read a, Show a) => Tag -> a -> NiceMap -> NiceMap
store tag a nm@(NiceMap {..}) =
  let mapp' = M.insert tag (show a) mapp
      cc' = nm { mapp = mapp' }
   in cc'

insert :: (Read a, Show a) => a -> NiceMap -> (Tag, NiceMap)
insert a nm =
  let (tag, nm') = alloc nm
      nm'' = store tag a nm'
   in (tag, nm'')

lookup :: (Read a, Show a) => Tag -> NiceMap -> Maybe a
lookup tag cc = read <$> M.lookup tag (mapp cc)

delete :: Tag -> NiceMap -> NiceMap
delete tag cc = cc { mapp = M.delete tag (mapp cc) }
