{-# Language GADTs, KindSignatures, NamedFieldPuns, TypeApplications #-}

module Log
( logMain
) where

import Data.Dynamic
import Data.Kind (Type)
import Data.Maybe
import Data.Tuple
import Type.Reflection
import Unsafe.Coerce

import Lift
import Monad
import Propagate
import Step
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
-- + V w
--   + rid of Ty.R
--     - pattern synonyms: https://gitlab.haskell.org/ghc/ghc/-/issues/8753
--   x then pat syns for the other ones (bi, vroot etc below
--   x OR don't pattern match the Rs, just add an op to write to it
--   x what about backpack?
-- - try factoring Dyn stuff
-- - main loop
--   - apply continuation
--   - Tmi monad (accumulate writes; take Step)
--   - Step
--   - state (init W, Step list, retval list)
--   - TMI -> TMI w, Step too
-- - Don't like applyContinuation in Log's recon
-- - Don't like that I have VNamed names where they're declared and also in recon
-- - Tmi re-export module
-- - remove V prefix from V ctors
-- - tell ghci to load Dyn + Veq compiled?
-- - ooo: <> for R (not for its contents) and then you don't have to say rc c = ... (?)
-- - various lowercase 'q's
-- - rename BS/etc
-- - general renaming
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
recon "mkAStep" = toDyn (vnamed "mkAStep" mkAStep)
recon "applyContinuation" = toDyn (vnamed "applyContinuation"
  (applyContinuation :: Step W -> Retval -> TMI W ()))
-- recon "nope" = toDyn (vnamed "nope" nope)
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
bplus_ _ ra _ rb = mkR rc
  where rc c = let a = c `div` 2
                   b = c - a
                in write ra a <> write rb b

baa :: V Int
baa = VBiSeal (BiApp (bi (VNamed "aa" aa) (VNamed "aa_" aa_)) root)
-- BApp (VNamed "aa" aa) (VNamed "aa_" aa_) root
-- aa :: W -> Int
aa_ :: W -> R W -> R Int
aa_ w wr = mkR ir
  where ir aa = write wr $ w { aa }

bbb :: V Int
bbb = VBiSeal (BiApp (bi (VNamed "bb" bb) (VNamed "bb_" bb_)) root)
-- bbb = BApp (VNamed "bb" bb) (VNamed "bb_" bb_) root
-- bb :: W -> Int
bb_ :: W -> R W -> R Int
bb_ w wr = mkR ir
  where ir bb = write wr $ w { bb }

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ r = mkR r'
  where r' i = write r (i - 1)

root :: V W
root = VRoot
theWorld :: W
theWorld = W { aa = 13, bb = 100 }

mkAStep :: Step W
mkAStep = Step (return ()) (\_ -> return ())

vstep :: V (Step W)
vstep = VNamed "mkAStep" mkAStep

logMain = do
  -- works but can't do v->s->v?
  [_, vstep'] <- roundTrip recon vstep
  let retval = mkRetval ()
      vRetval = VNice retval
      vApplyContinuation = VNamed "applyContinuation" applyContinuation
      biApplyContinuation = uni vApplyContinuation
      liftedApplyContinuation = lift2 biApplyContinuation
      vTMI = liftedApplyContinuation vstep' vRetval
  msp vTMI
      -- tmi = applyContinuation step retval
      -- tmi' = applyContinuation step' retval

  -- Works
  -- msp $ propToRoots theWorld (Write added 140)
  -- msp $ propToRoots theWorld (Write added' 140)
  -- msp $ propToRoots theWorld (Write added'' 140)
  -- msp $ propToRoots theWorld (Write added''' 140)
  -- roundTrip recon vroot
  -- roundTrip recon added
  -- roundTrip recon added'
  -- roundTrip recon added''
  -- roundTrip recon added'''

  -- msp $ added == added
  -- msp $ added == added'
  -- msp $ added' == added'
  -- msp $ added' == added

  msp "log hi"

{-
data Dumms = Dumms Int

pr :: a -> (a, a)
pr x = (x, x)

hmm :: Dumms -> a -> b
hmm wtf x = (unsafeCoerce wtf) x

data Foo a b c = Foo a (b, c) deriving Show
fooSnd :: Foo a b c -> (b, c)
fooSnd (Foo _ p) = p

data WW = WW { foo :: Foo Int Float String } deriving Show
ww = WW { foo = Foo 12 (13.5, "hey") }

git :: String -> a
git "foo" = unsafeCoerce foo
git "fooSnd" = unsafeCoerce fooSnd
git "swap" = unsafeCoerce swap

type VV = Ty.V WW
wwroot :: VV WW
wwroot = VRoot

_foo :: VV WW -> VV (Foo Int Float String)
_foo = lift1 $ Ty.Bi (VNamed "foo" foo) undefined
-- _fooSnd :: VV (Foo Int Float String) -> VV (Float, String)
_fooSnd :: (Typeable a, Typeable b, Typeable c) => VV (Foo a b c) -> VV (b, c)
_fooSnd = lift1 $ Ty.Bi (VNamed "fooSnd" fooSnd) undefined
_swap :: (Typeable a, Typeable b) => VV (a, b) -> VV (b, a)
-- _swap :: VV (a, b) -> VV (b, a)
_swap = lift1 $ Ty.Bi (VNamed "swap" swap) undefined

vgit :: {-Typeable a =>-} String -> a
vgit "_foo" = unsafeCoerce _foo
-- vgit "_fooSnd" = unsafeCoerce _fooSnd
-- vgit "_swap" = unsafeCoerce _swap

tid :: Typeable a => a -> a
tid x = x

dummMain = do
  -- works
  -- msp $ rd ww wwroot
  -- msp $ rd ww (_foo wwroot)
  -- msp $ rd ww (_fooSnd (_foo wwroot))
  -- msp $ rd ww (_swap (_fooSnd (_foo wwroot)))
  let vf = vgit "_foo" wwroot :: VV (Foo Int Float String)
      vpr = vgit "_fooSnd" $ vgit "_foo" wwroot :: VV (Float, String)
      -- vprs = vgit "_swap" $ vgit "_fooSnd" $ vgit "_foo" wwroot :: VV (String, Float)
  msp $ rd ww vf
  msp $ rd ww vpr
  -- msp $ rd ww vprs

  -- works
  -- let fs = git "foo" ww :: Foo Int Float String
  --     pr = git "fooSnd" $ git "foo" ww :: (Float, String)
  --     prs = git "swap" $ git "fooSnd" $ git "foo" ww :: (Float, String)
  --     prs' = git "swap" pr :: (String, Float)
  -- msp fs
  -- msp pr
  -- msp prs
  -- msp prs'

  -- let dumpr = unsafeCoerce pr :: Dumms
  --     n = 12 :: Int
  --     nn = hmm dumpr n :: (Int, Int)
  -- msp nn
  msp "ho"
-}
