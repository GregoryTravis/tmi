{-# Language GADTs, NamedFieldPuns, TypeApplications #-}

module Log
( logMain
) where

import Data.Dynamic

import Lift
import Propagate
import Storage
import Testing
import Ty hiding (V, Bi, R)
import qualified Ty as Ty
import Util
import V
import Veq

-- todo
-- + dead: VApp, <$$>, faa, inced_
-- + w -> theWorld (cuz it's often a param that I sometimes forget to pass)
-- + move typerep stuff to another file so we don't have to rebuild all the time
-- + lifters and use them for sepps
-- + Eq for V -- need BiApp
-- + roundTrip asserts they're equal
-- + remove most everything else
-- + modules: propagate, serialization, rd/wr, testlib (roundTrip)
-- + Rename to V
-- - V w
--   - rid of Ty.R
--     - pattern synonyms: https://gitlab.haskell.org/ghc/ghc/-/issues/8753
--   - then pat syns for the other ones (bi, vroot etc below(
--   - what about backpack?
-- - tell ghci to load Dyn + Veq compiled?
-- - ooo: <> for R (not for its contents) and then you don't have to say rc c = ... (?)
-- - various lowercase 'q's
-- - rename BS/etc
-- - general renaming
-- - main loop
--   - Tmi monad (accumulate writes; take Step)
--   - Step
--   - state (init W, Step list, retval list)
-- - multi-module registry

data W = W { aa :: Int, bb :: Int } deriving (Read, Show)

type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W
bi :: (Typeable f, Typeable r) => V f -> V r -> Bi f r
bi = Ty.Bi
vnamed :: String -> a -> Ty.V W a
vnamed = Ty.VNamed
vroot :: V W
vroot = Ty.VRoot

recon :: String -> Dynamic
recon "aa" = toDyn (vnamed "aa" aa)
recon "aa_" = toDyn (vnamed "aa_" aa_)
recon "bb" = toDyn (vnamed "bb" bb)
recon "bb_" = toDyn (vnamed "bb_" bb_)
recon "inc" = toDyn (vnamed "inc" inc)
recon "inc_" = toDyn (vnamed "inc_" inc_)
recon "bplus" = toDyn (vnamed "bplus" bplus)
recon "bplus_" = toDyn (vnamed "bplus_" bplus_)
recon s = error $ show ("recon", s)

addEmBi :: Bi (Int -> Int -> Int)
              (Int -> R Int -> Int -> R Int -> R Int)
addEmBi = bi (VNamed "bplus" bplus) (VNamed "bplus_" bplus_)
addEm = lift2 addEmBi

plursBi :: Bi (Int -> Int) (Int -> R Int -> R Int)
plursBi = bi (VNamed "inc" inc) (VNamed "inc_" inc_)
plurs = lift1 plursBi

added = addEm baa bbb
added' = addEm (plurs baa) bbb
added'' = addEm baa (plurs bbb)
added''' = addEm (plurs baa) (plurs bbb)

bplus :: Int -> Int -> Int
bplus = (+)
bplus_ :: Int -> R Int -> Int -> R Int -> R Int
bplus_ _ (Ty.R ra) _ (Ty.R rb) = Ty.R rc
  where rc c = let a = c `div` 2
                   b = c - a
                in ra a <> rb b

baa :: V Int
baa = VBiSeal (BiApp (bi (VNamed "aa" aa) (VNamed "aa_" aa_)) root)
-- BApp (VNamed "aa" aa) (VNamed "aa_" aa_) root
-- aa :: W -> Int
aa_ :: W -> R W -> R Int
aa_ w (Ty.R wr) = Ty.R ir
  where ir aa = wr $ w { aa }

bbb :: V Int
bbb = VBiSeal (BiApp (bi (VNamed "bb" bb) (VNamed "bb_" bb_)) root)
-- bbb = BApp (VNamed "bb" bb) (VNamed "bb_" bb_) root
-- bb :: W -> Int
bb_ :: W -> R W -> R Int
bb_ w (Ty.R wr) = Ty.R ir
  where ir bb = wr $ w { bb }

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ (Ty.R r) = Ty.R r'
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
  roundTrip recon vroot
  roundTrip recon added
  roundTrip recon added'
  roundTrip recon added''
  roundTrip recon added'''

  msp $ added == added
  msp $ added == added'
  msp $ added' == added'
  msp $ added' == added

  msp "log hi"
