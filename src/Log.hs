{-# Language GADTs, KindSignatures, NamedFieldPuns, TypeApplications #-}

module Log
( logMain
) where

-- import Data.Dynamic
-- import Data.Kind (Type)
import Data.Maybe
import Data.Tuple
-- import Type.Reflection
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
bi :: V f -> V r -> Bi f r
bi = Ty.Bi
vnamed :: String -> a -> Ty.V W a
vnamed = Ty.VNamed
vroot :: V W
vroot = Ty.VRoot

-- recon :: String -> Dynamic
-- recon "aa" = toDyn (vnamed "aa" aa)
-- recon "aa_" = toDyn (vnamed "aa_" aa_)
-- recon "bb" = toDyn (vnamed "bb" bb)
-- recon "bb_" = toDyn (vnamed "bb_" bb_)
-- recon "inc" = toDyn (vnamed "inc" inc)
-- recon "inc_" = toDyn (vnamed "inc_" inc_)
-- recon "bplus" = toDyn (vnamed "bplus" bplus)
-- recon "bplus_" = toDyn (vnamed "bplus_" bplus_)
-- recon "mkAStep" = toDyn (vnamed "mkAStep" mkAStep)
-- recon "applyContinuation" = toDyn (vnamed "applyContinuation"
--   (applyContinuation :: Step W -> Retval -> TMI W ()))
-- -- recon "nope" = toDyn (vnamed "nope" nope)
-- recon s = error $ show ("recon", s)

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

recon :: String -> a
recon "aa" = unsafeCoerce $ VNamed "aa" aa
recon "aa_" = unsafeCoerce $ VNamed "aa_" aa_
recon "bb" = unsafeCoerce $ VNamed "bb" bb
recon "bb_" = unsafeCoerce $ VNamed "bb_" bb_
recon "bplus" = unsafeCoerce $ VNamed "bplus" bplus
recon "bplus_" = unsafeCoerce $ VNamed "bplus_" bplus_
recon "inc" = unsafeCoerce $ VNamed "inc" inc
recon "inc_" = unsafeCoerce $ VNamed "inc_" inc_
recon "mkAStep" = unsafeCoerce $ VNamed "mkAStep" mkAStep
recon "applyContinuation" = unsafeCoerce $ VNamed "applyContinuation" applyContinuation
recon "nope" = unsafeCoerce $ VNamed "nope" nope
recon s = error $ "recon?? " ++ s

foo :: Int -> Int
foo x = x * 2

fooK :: Int -> (Int -> r) -> r
fooK x k = k (x * 2)
fooK' :: Int -> (Int -> r) -> r
fooK' = klift foo

klift :: (a -> b) -> (a -> (b -> r) -> r)
klift f = \x k -> k (f x)

-- here's the method
-- a in double negative position means you can magically produce an a inside an application
-- foo :: (a -> b) -> c
-- foo a2b = a2b (\a -> ... use a to produce c)

-- (>>=) :: forall a b. m a -> (a -> m b) -> m b
data C r a = C ((a -> r) -> r)
-- ((a -> r) -> r) >>= (a -> (b -> r) -> r) :: C r b
boond :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> ((b -> r) -> r)
boond kfa a2kfb = \b2r -> kfa ((flip a2kfb) b2r)

boondC :: C r a -> (a -> C r b) -> C r b
-- yikes
-- boondC (C kfa) a2Crb = C (\b2r -> kfa (\a -> (case (a2Crb a) of C br2r -> br2r) b2r))
-- still yikes
boondC (C kfa) a2Crb = C foo
  where foo b2r = kfa bar
          where bar a = -- (case (a2Crb a) of C br2r -> br2r) b2r
                  let C br2r = a2Crb a
                   in br2r b2r

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> C ((a -> r) -> r) -> C ((b -> r) -> r)
-- so much yikes
instance Functor (C r) where
  fmap ab (C ar2r) = C br2r
    where br2r b2r = ar2r (\a -> b2r (ab a))

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b 
-- (<*>) :: C r (a -> b) -> C r a -> C r b 
-- (<*>) :: C (((a -> b) -> r) -> r) -- a2b2r2r
--       -> C ((a -> r) -> r)        -- a2r2r
--       -> C ((b -> r) -> r)        -- b2r2r
-- yikes out the wazoo
instance Applicative (C r) where
  pure x = C (\k -> k x)
  C a2b2r2r <*> C a2r2r =
    let b2r2r b2r =  -- return an r
          a2b2r2r (\a2b -> a2r2r (\a -> b2r (a2b a)))
          -- a2b2r2r (\a2b2r -> a2b2r (\a2b -> a2r2r (\a -> b2r (a2b a))))
     in C b2r2r

instance Monad (C r) where
  (>>=) = boondC

logMain = do
  msp $ fooK 12 (\x -> show $ "yep " ++ show x)
  msp $ fooK' 13 (\x -> show $ "yep " ++ show x)

  -- works but can't do v->s->v?
  [_, vstep'] <- roundTrip recon vstep
  let retval = mkRetval ()
      vRetval = VNice retval
      vApplyContinuation = VNamed "applyContinuation" applyContinuation
      biApplyContinuation = uni vApplyContinuation
      liftedApplyContinuation = lift2 biApplyContinuation
      vTMI = liftedApplyContinuation vstep' vRetval
  -- msp vRetval
  -- msp $ qs vRetval
  -- msp $ ((unqs recon $ qs $ vRetval) :: V Retval)
  msp vTMI
  roundTrip recon vTMI >>= msp
  -- ya <- roundTrip recon vRetval :: IO [V Retval]
  -- msp ya

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
  -- msp $ added' == added''
  -- msp $ added' == added'''

  -- works, or rather did before I split the recon bis
  -- msp added -- just aa + bb
  -- let baaa = BiApp (unsafeCoerce (recon "aa")) VRoot
  --     slaa = VBiSeal baaa
  --     babb = BiApp (unsafeCoerce (recon "bb")) VRoot
  --     slbb = VBiSeal babb
  --     pl = VBiSeal (BiApp (BiApp (unsafeCoerce (recon "bplus")) slaa) slbb)
  -- msp pl
  -- msp $ show added == show pl
  -- msp $ added == pl
  -- msp $ added == slaa

  msp "log hi"
