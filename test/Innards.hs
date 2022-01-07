{-# Language NamedFieldPuns #-}

module Innards where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Lift
import Propagate
import Ty hiding (V, Bi, R)
import qualified Ty as Ty
import V

-- Some low-level prodding

data W = W { aa :: Int, bb :: Int } deriving (Eq, Read, Show)
type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W
bi :: V f -> V r -> Bi f r
bi = Ty.Bi
root :: V W
root = VRoot

theWorld :: W
theWorld = W { aa = 13, bb = 100 }

addEmBi :: Bi (Int -> Int -> Int)
              (Int -> R Int -> Int -> R Int -> R Int)
addEmBi = bi (VNamed "bplus" bplus) (VNamed "bplus_" bplus_)
addEm = lift2 addEmBi

added = addEm baa bbb
added' = addEm (plurs baa) bbb
added'' = addEm baa (plurs bbb)
added''' = addEm (plurs baa) (plurs bbb)

plursBi :: Bi (Int -> Int) (Int -> R Int -> R Int)
plursBi = bi (VNamed "inc" inc) (VNamed "inc_" inc_)
plurs = lift1 plursBi

bplus :: Int -> Int -> Int
bplus = (+)
bplus_ :: Int -> R Int -> Int -> R Int -> R Int
bplus_ _ ra _ rb = mkR rc
  where rc c = let a = c `div` 2
                   b = c - a
                in write ra a <> write rb b

baa :: V Int
baa = VBiSeal (BiApp (bi (VNamed "aa" aa) (VNamed "aa_" aa_)) root)
aa_ :: W -> R W -> R Int
aa_ w wr = mkR ir
  where ir aa = write wr $ w { aa }

bbb :: V Int
bbb = VBiSeal (BiApp (bi (VNamed "bb" bb) (VNamed "bb_" bb_)) root)
bb_ :: W -> R W -> R Int
bb_ w wr = mkR ir
  where ir bb = write wr $ w { bb }

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ r = mkR r'
  where r' i = write r (i - 1)

innardsSuite :: TestTree
innardsSuite = testGroup "Test Suite" [
  testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= GT

  , testCase "" $
    -- (propWrite theWorld (Write added 140)) `compare` "asdf" @?= EQ
    (propWrite theWorld (Write added 140)) @?= (W { aa = 70 , bb = 70 })
  -- msp $ propWrite theWorld (Write added' 140)
  -- msp $ propWrite theWorld (Write added'' 140)
  -- msp $ propWrite theWorld (Write added''' 140)
  -- msp $ added == added
  -- msp $ added /= added'
  -- msp $ added' == added'
  -- msp $ added' /= added
  -- msp $ added' /= added''
  -- msp $ added' /= added'''

  -- -- works, or rather did before I split the recon bis
  -- let baaa = BiApp (Ty.Bi (VNamed "aa" aa) (VNamed "aa_" aa_)) VRoot
  --     slaa = VBiSeal baaa
  --     babb = BiApp (Ty.Bi (VNamed "bb" bb) (VNamed "bb_" bb_)) VRoot
  --     slbb = VBiSeal babb
  --     pl = VBiSeal (BiApp (BiApp (Ty.Bi (unsafeCoerce (recon "bplus")) (unsafeCoerce (recon "bplus_"))) slaa) slbb)
  -- let lala = "(VBiSeal (BiApp (BiApp (Bi (VNamed bplus) (VNamed bplus_)) (VBiSeal (BiApp (Bi (VNamed aa) (VNamed aa_)) VRoot))) (VBiSeal (BiApp (Bi (VNamed bb) (VNamed bb_)) VRoot))))"
  -- msp $ show pl == lala
  -- msp $ show added == show pl
  -- msp $ added == pl
  -- msp $ added /= slaa
  ]
