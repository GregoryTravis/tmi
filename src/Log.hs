{-# Language GADTs, KindSignatures, NamedFieldPuns, RecordWildCards, TypeApplications #-}

module Log
( logMain
) where

-- import Data.Dynamic
-- import Data.Kind (Type)
import Control.Concurrent
import Data.Maybe
import Data.Tuple
-- import Type.Reflection
import System.Directory
import System.Environment
import Unsafe.Coerce

import Lift
import Monad
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
-- + V w
--   + rid of Ty.R
--     - pattern synonyms: https://gitlab.haskell.org/ghc/ghc/-/issues/8753
--   x then pat syns for the other ones (bi, vroot etc below
--   x OR don't pattern match the Rs, just add an op to write to it
--   x what about backpack?
-- - oh shit you need some pragmas etc for unsafePerformIO
--   - https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-IO.html
--   - scroll down a bit
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

recon :: String -> a
recon "aa" = unsafeCoerce $ VNamed "aa" aa
recon "aa_" = unsafeCoerce $ VNamed "aa_" aa_
recon "bb" = unsafeCoerce $ VNamed "bb" bb
recon "bb_" = unsafeCoerce $ VNamed "bb_" bb_
recon "bplus" = unsafeCoerce $ VNamed "bplus" bplus
recon "bplus_" = unsafeCoerce $ VNamed "bplus_" bplus_
recon "inc" = unsafeCoerce $ VNamed "inc" inc
recon "inc_" = unsafeCoerce $ VNamed "inc_" inc_
recon "nope" = unsafeCoerce $ VNamed "nope" nope
recon s = error $ "recon?? " ++ s

data Retval = Retval Int String

program :: Program W
program = Program
  [ Assign (Write baa 140)
  , Call (Step (msp "hey")
         (\() -> Program [Call (Step (msp "hey2") (\() -> Program [Done]))]))
  , Call (Step (listDirectory ".")
         (\files -> Program [Call (Step (msp files) (\() -> Program [Done]))]))
  , Assign (Write bbb 1111)
  ]

data Eternity w = Eternity
  { retvals :: [Retval]
  , initW :: w
  , initStep :: Step w
  , nextStep :: Int
  , retvalChan :: Chan Retval
  }

data History w = History
  { ws :: [w]
  , hRetvals :: [Retval]
  , steps :: [Step w]
  }

data Generation w = Generation
  { writes :: [Write w]
  , newSteps :: [Step w]
  }

mkEternity :: Chan Retval -> w -> ([String] -> Program w) -> Eternity w
mkEternity chan initW tmiMain = Eternity
  { retvals = []
  , initW = initW
  , initStep = Step getArgs tmiMain
  , nextStep = 0
  , retvalChan = chan
  }

startHistory :: Eternity w -> History w
startHistory (Eternity { retvals, initW, initStep }) =
  History { ws = [initW]
          , hRetvals = retvals
          , steps = [initStep]
          }

endHistory :: Eternity w -> History w -> Eternity w
endHistory e@(Eternity { nextStep, retvals }) (History { steps, hRetvals }) =
  e { nextStep = nextStep', retvals = retvals' }
  where nextStep' = length steps
        retvals' = hRetvals

startGeneration :: History w -> Generation w
startGeneration _ = Generation { writes = [], newSteps = [] }

endGeneration :: Show w => History w -> Generation w -> History w
endGeneration h@(History { ws }) (Generation { writes, newSteps }) =
  h { ws = ws' }
  where ws' = ws ++ [newW]
        newW = propToRoot (last ws) (Writes writes)

runProgram :: Generation w -> Program w -> Generation w
runProgram gen (Program cores) = runProgramSteps gen cores
runProgramSteps :: Generation w -> [Core w] -> Generation w
runProgramSteps gen (c:cs) = runProgramSteps (runProgramStep gen c) cs
runProgramSteps gen [] = gen
runProgramStep :: Generation w -> Core w -> Generation w
runProgramStep gen (Assign write) = gen { writes = writes gen ++ [write] }
runProgramStep gen (Call step) = gen { newSteps = newSteps gen ++ [step] }
runProgramStep gen Done = gen

startLoop :: w -> ([String] -> Program w) -> IO ()
startLoop initW tmiMain = do
  chan <- newChan
  args <- getArgs
  let primalRetval = Retval 0 (show args)
  writeChan chan primalRetval
  mainLoop (mkEternity chan initW tmiMain)

mainLoop :: Eternity w -> IO ()
mainLoop eternity =
  -- wait for retval, add it to the list
  -- refresh
  --   - handle new retvals (propagate)
  --   - handle new steps (fork)
  mainLoop eternity

logMain = do
  -- finalWorld <- runProgram theWorld program
  -- msp finalWorld

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
