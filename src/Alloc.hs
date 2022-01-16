{-# Language GADTs, NamedFieldPuns, QualifiedDo #-}

module Alloc
( Alloc
, Allocated(..)
, mkAlloc
, alloc
, dealloc ) where

import Prelude hiding (map)
import Data.IntMap hiding (map)

import Core
import Lift
import Monad
import Ty
import Util
import V

-- I will burn in hell for creating something that requires explicit deallocation.
data Alloc a = Alloc
  { map :: IntMap a
  , serial :: Int } deriving (Read, Show)

-- The allocated thing and a deallocator
-- Again, I shall be punished for this
data Allocated w a = Allocated (V w a) (Blef w ())
  deriving (Show)

slot :: Alloc a -> Int -> a
slot alloc i = feesp "AAA read" $ map alloc ! i
slot_ :: Alloc a -> R w (Alloc a) -> Int -> R w Int -> R w a
slot_ alloc ralloc i _ = mkR r
  where r a = write ralloc alloc'
              where alloc' = feesp "AAA insert" $ alloc { map = insert i a (map alloc) }
vslot :: V w (Alloc a) -> V w Int -> V w a
vslot = lift2 $ bi (VNamed "slot" slot) (VNamed "slot_" slot_)

-- inc :: Int -> Int
-- inc = (+1)
-- inc_ :: Int -> R Int -> R Int
-- inc_ _ r = mkR r'
--   where r' i = write r (i - 1)
-- incV :: V Int -> V Int
-- incV = lift1 $ bi (VNamed "inc" inc) (VNamed "inc_" inc_)

mkAlloc :: Alloc a
mkAlloc = Alloc { map = empty, serial = 0 }

-- alloc = undefined
-- dealloc = undefined

-- TODO: Why do we need to read here?
alloc :: V w (Alloc a) -> a -> Blef w (Allocated w a)
alloc valloc initialValue = do
  alloc <- BRead valloc
  let va = vslot valloc (VNice (serial alloc))
      i = serial alloc
      map' = insert i initialValue (map alloc)
      alloc' = alloc { map = map', serial = i + 1 }
      allocated = Allocated va (dealloc valloc i)
  BWrite valloc alloc'
  return allocated

dealloc :: V w (Alloc a) -> Int -> Blef w ()
dealloc valloc i = do
  alloc <- BRead valloc
  let alloc' = alloc { map = map' }
      map' = delete i (map alloc)
  BWrite valloc alloc'
