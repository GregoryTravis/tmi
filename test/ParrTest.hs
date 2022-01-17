{-# Language NamedFieldPuns, QualifiedDo #-}

module ParrTest (parrSuite) where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Unsafe.Coerce

import Alloc
import Core
import Lift
import Monad
import Parr
import Propagate
import Ty hiding (V, Bi, R)
import qualified Ty as Ty
import V
import Veq

import TestUtil

-- Some low-level prodding

data W = W { allocator :: Alloc } deriving (Read, Show)

type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W

root :: V W
root = VRoot

vallocator :: V Alloc
vallocator = VBiSeal (BiApp (bi (VNamed "allocator" allocator)
                                    (VNamed "allocator_" allocator_)) root)
allocator_ :: W -> R W -> R Alloc
allocator_ w wr = mkR ir
  where ir allocator = write wr $ w { allocator }

-- parrTest :: Program W
-- parrTest = toProg done $ M.do
--   let blef0 = M.do M.return 1
--       blef1 = M.do M.return "asdf"
--   (i, s) <- parr vpairAllocator blef0 blef1
--   return (i, s)

-- TODO finish this
parrSuite :: TestTree
parrSuite = testGroup "Test Suite" [
    -- (1, "asdf") ~?= 
  ]
