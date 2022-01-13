{-# Language GADTs, NamedFieldPuns, QualifiedDo #-}

module Alloc
( Alloc
, mkAlloc
, alloc
, dealloc ) where

import Prelude hiding (map)
import Data.IntMap hiding (map)

import Core
import Lift
import qualified Monad as M
import Ty
import Util
import V

-- I will burn in hell for creating something that requires explicit deallocation.
data Alloc a = Alloc
  { map :: IntMap a
  , serial :: Int } deriving Show

-- The allocated thing and a deallocator
-- Again, I shall be punished for this
data Allocated w a = Allocated (V w a) (Blef w ())
  deriving (Show)

slot :: Alloc a -> Int -> a
slot alloc i = map alloc ! i
slot_ :: Alloc a -> R w (Alloc a) -> Int -> R w Int -> R w a
slot_ alloc ralloc i _ = mkR r
  where r a = write ralloc alloc'
              where alloc' = alloc { map = insert i a (map alloc) }
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

alloc = undefined
dealloc = undefined

-- TODO: Why do we need to read here?
-- alloc :: (Show a) => V w (Alloc a) -> Blef w (Allocated w a)
-- alloc valloc = M.do
--   alloc <- BRead valloc
--   let va = vslot valloc (VNice (serial alloc))
--       alloc' = alloc { serial = serial alloc + 1 }
--       allocated = Allocated va (dealloc valloc (serial alloc))
--   BWrite valloc alloc'
--   M.return (return allocated)

-- dealloc :: (Read a, Show a) => V w (Alloc a) -> Int -> Blef w ()
-- dealloc valloc i = M.do
--   alloc <- BRead valloc
--   let alloc' = alloc { map = map' }
--       map' = delete i (map alloc)
--   BWrite valloc alloc'
