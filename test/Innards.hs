{-# Language FlexibleInstances, NamedFieldPuns #-}

module Innards (innardsSuite) where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Unsafe.Coerce

import Lift
import Propagate
import Storage
import Ty hiding (V, Bi, R, W)
import qualified Ty as Ty
import V
import Veq

infix 1 ~?=
a ~?= e = testCase "" $ a @?= e

-- Some low-level prodding

data W = W { aa :: Int, bb :: Int } deriving (Eq, Read, Show)
type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W
root :: V W
root = VRoot

instance Show (Ty.V W a) where
  show v = show (qs v)

instance Read (Ty.V W a) where
  readsPrec i s = readsPrecer recon i s

theWorld :: W
theWorld = W { aa = 13, bb = 100 }

add :: V Int -> V Int -> V Int
add = lift2 $ bi (VNamed "bplus" bplus) (VNamed "bplus_" bplus_)

added = add baa bbb
added' = add (incV baa) bbb
added'' = add baa (incV bbb)
added''' = add (incV baa) (incV bbb)

incV :: V Int -> V Int
incV = lift1 $ bi (VNamed "inc" inc) (VNamed "inc_" inc_)

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

recon :: String -> a
recon "aa" = unsafeCoerce $ VNamed "aa" aa
recon "aa_" = unsafeCoerce $ VNamed "aa_" aa_
recon "bb" = unsafeCoerce $ VNamed "bb" bb
recon "bb_" = unsafeCoerce $ VNamed "bb_" bb_
recon "bplus" = unsafeCoerce $ VNamed "bplus" bplus
recon "bplus_" = unsafeCoerce $ VNamed "bplus_" bplus_
recon "inc" = unsafeCoerce $ VNamed "inc" inc
recon "inc_" = unsafeCoerce $ VNamed "inc_" inc_
-- recon "nope" = unsafeCoerce $ VNamed "nope" nope
-- recon "smallProg" = unsafeCoerce $ VNamed "smallProg" smallProgBlef
recon s = error $ "recon?? " ++ s

innardsSuite :: TestTree
innardsSuite = testGroup "Test Suite" [
    propWrite theWorld (Write added 140) ~?= W { aa = 70 , bb = 70 }
  , propWrite theWorld (Write added' 140) ~?= W { aa = 69 , bb = 70 }
  , propWrite theWorld (Write added'' 140) ~?= W { aa = 70 , bb = 69 }
  , propWrite theWorld (Write added''' 140) ~?= W { aa = 69 , bb = 69 }

  , added ~?= added
  , added /= added' ~?= True
  , added' ~?= added'
  , added' /= added ~?= True
  , added' /= added'' ~?= True
  , added' /= added''' ~?= True

  , let pl = VBiSeal (BiApp (BiApp (Ty.Bi (unsafeCoerce (recon "bplus")) (unsafeCoerce (recon "bplus_"))) baa) bbb)
        lala = "SVBiSeal (BSBiApp (BSBiApp (BSBi (SNamed \"bplus\") (SNamed \"bplus_\")) (SVBiSeal (BSBiApp (BSBi (SNamed \"aa\") (SNamed \"aa_\")) SRoot))) (SVBiSeal (BSBiApp (BSBi (SNamed \"bb\") (SNamed \"bb_\")) SRoot)))"
     in testGroup "" [
          show pl ~?= lala
        , show added ~?= show pl
        , added ~?= pl
        , added /= baa ~?= True
        ]
  ]
