{-# Language GADTs, NamedFieldPuns #-}

module Log
( logMain
) where

import Data.Dynamic

import Lift
import Propagate
import V
import Veq
import Storage
import Testing
import Ty
import Util

-- todo
-- + dead: VApp, <$$>, faa, inced_
-- + w -> theWorld (cuz it's often a param that I sometimes forget to pass)
-- + move typerep stuff to another file so we don't have to rebuild all the time
-- + lifters and use them for sepps
-- + Eq for V -- need BiApp
-- + roundTrip asserts they're equal
-- + remove most everything else
-- + modules: propagate, serialization, rd/wr, testlib (roundTrip)
-- - Rename to V
-- - V w
-- - main loop
--   - Tmi monad (accumulate writes; take Step)
--   - Step
--   - state (init W, Step list, retval list)
-- - multi-module registry

recon :: String -> Dynamic
recon "aa" = toDyn (VNamed "aa" aa)
recon "aa_" = toDyn (VNamed "aa_" aa_)
recon "bb" = toDyn (VNamed "bb" bb)
recon "bb_" = toDyn (VNamed "bb_" bb_)
recon "inc" = toDyn (VNamed "inc" inc)
recon "inc_" = toDyn (VNamed "inc_" inc_)
recon "bplus" = toDyn (VNamed "bplus" bplus)
recon "bplus_" = toDyn (VNamed "bplus_" bplus_)
recon s = error $ show ("recon", s)

addEmBi :: Bi (Int -> Int -> Int)
            (Int -> R Int -> Int -> R Int -> R Int)
addEmBi = Bi (VNamed "bplus" bplus) (VNamed "bplus_" bplus_)
addEm = lift2 addEmBi

plursBi :: Bi (Int -> Int)
            (Int -> R Int -> R Int)
plursBi = Bi (VNamed "inc" inc) (VNamed "inc_" inc_)
plurs = lift1 plursBi

added = addEm baa bbb
added' = addEm (plurs baa) bbb
added'' = addEm baa (plurs bbb)
added''' = addEm (plurs baa) (plurs bbb)

bplus :: Int -> Int -> Int
bplus = (+)
bplus_ :: Int -> R Int -> Int -> R Int -> R Int
bplus_ _ (R ra) _ (R rb) = R rc
  where rc c = let a = c `div` 2
                   b = c - a
                in ra a <> rb b

baa :: V Int
baa = VBiSeal (BiApp (Bi (VNamed "aa" aa) (VNamed "aa_" aa_)) root)
-- BApp (VNamed "aa" aa) (VNamed "aa_" aa_) root
-- aa :: W -> Int
aa_ :: W -> R W -> R Int
aa_ w (R wr) = R ir
  where ir aa = wr $ w { aa }

bbb :: V Int
bbb = VBiSeal (BiApp (Bi (VNamed "bb" bb) (VNamed "bb_" bb_)) root)
-- bbb = BApp (VNamed "bb" bb) (VNamed "bb_" bb_) root
-- bb :: W -> Int
bb_ :: W -> R W -> R Int
bb_ w (R wr) = R ir
  where ir bb = wr $ w { bb }

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ (R r) = R r'
  where r' i = r (i - 1)

root :: V W
root = VRoot
theWorld :: W
theWorld = W { aa = 13, bb = 100 }

logMain = do
  -- Works
  msp $ propToRoots theWorld (Write added 140)
  msp $ propToRoots theWorld (Write added' 140)
  msp $ propToRoots theWorld (Write added'' 140)
  msp $ propToRoots theWorld (Write added''' 140)
  roundTrip recon VRoot
  roundTrip recon added
  roundTrip recon added'
  roundTrip recon added''
  roundTrip recon added'''

  msp $ added == added
  msp $ added == added'
  msp $ added' == added'
  msp $ added' == added

  msp "log hi"
