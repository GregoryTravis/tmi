{-# Language GADTs, FlexibleContexts, KindSignatures, NamedFieldPuns, NumericUnderscores,
   RecordWildCards, ScopedTypeVariables, TypeApplications #-}

module Log
( logMain
) where

-- import Data.Dynamic
-- import Data.Kind (Type)
import Control.Concurrent
import Control.Monad (forM_)
import Data.Maybe
import Data.Tuple
-- import Type.Reflection
import System.Directory
import System.Environment
import System.Random
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
-- - main loop
--   - wait, why are we passing args to main, this is not Java
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

data Retval = Retval Int String deriving Show

data Eternity w = Eternity
  { retvals :: [Retval]
  , initW :: w
  , initStep :: Step w
  , nextStep :: Int
  , retvalChan :: Chan Retval
  , monitorings :: [Monitoring w]
  }

data History w = History
  { ws :: [w]
  , hRetvals :: [Retval]
  , steps :: [Step w]
  , hNewMonitorings :: [Monitoring w]
  }

data Generation w = Generation
  { writes :: [Write w]
  , newSteps :: [Step w]
  , gNewMonitorings :: [Monitoring w]
  }

instance Show w => Show (Eternity w) where
  show (Eternity {..}) = show ("Eternity", retvals, initW, nextStep)
instance Show w => Show (History w) where
  show (History {..}) = show ("History", hRetvals, ws, length steps)
instance Show (Generation w) where
  show (Generation {..}) = show ("Generation", writes, length newSteps)

mkEternity :: Chan Retval -> w -> ([String] -> Program w) -> Eternity w
mkEternity chan initW tmiMain = Eternity
  { retvals = []
  , initW = initW
  , initStep = Step getArgs tmiMain
  , nextStep = 1
  , retvalChan = chan
  , monitorings = []
  }

startHistory :: Eternity w -> History w
startHistory (Eternity { retvals, initW, initStep }) =
  History { ws = [initW]
          , hRetvals = retvals
          , steps = [initStep]
          , hNewMonitorings = []
          }

endHistory :: Eternity w -> History w -> (w, Eternity w)
endHistory e@(Eternity { nextStep, retvals }) h@(History { steps, hRetvals }) =
  (last (ws h), e')
  where e' = e { nextStep = nextStep', retvals = retvals', monitorings = monitorings' }
        nextStep' = length steps
        retvals' = hRetvals
        monitorings' = monitorings e ++ hNewMonitorings h

startGeneration :: History w -> Generation w
startGeneration _ = Generation { writes = [], newSteps = [], gNewMonitorings = [] }

endGeneration :: Show w => History w -> Generation w -> History w
endGeneration h@(History { ws }) g@(Generation { writes, newSteps }) =
  h { ws = ws', steps = newSteps', hNewMonitorings = monitorings' }
  where ws' = ws ++ [newW]
        newW = propToRoot (last ws) (Writes writes)
        newSteps' = steps h ++ newSteps
        monitorings' = hNewMonitorings h ++ gNewMonitorings g

runProgram :: Generation w -> Program w -> Generation w
runProgram gen (Program cores) = runProgramSteps gen cores
runProgramSteps :: Generation w -> [Core w] -> Generation w
runProgramSteps gen (c:cs) = runProgramSteps (runProgramStep gen c) cs
runProgramSteps gen [] = gen
runProgramStep :: Generation w -> Core w -> Generation w
runProgramStep gen step = runProgramStep' gen step
runProgramStep' gen (Assign write) = gen { writes = writes gen ++ [write] }
runProgramStep' gen (Call step) = gen { newSteps = newSteps gen ++ [step] }
runProgramStep' gen (Mon mon) =
  gen { gNewMonitorings = gNewMonitorings gen ++ [mon] }
runProgramStep' gen Done = gen

startLoop :: Show w => w -> ([String] -> Program w) -> IO ()
startLoop initW tmiMain = do
  chan <- newChan
  args <- getArgs
  let primalRetval = Retval 0 (show args)
  writeChan chan primalRetval
  mainLoop (mkEternity chan initW tmiMain)

mainLoop :: forall w. Show w => Eternity w -> IO ()
mainLoop e = do
  -- wait for retval, add it to the list
  msp "readChan"
  newRetval <- readChan (retvalChan e)
  msp $ "got " ++ show newRetval
  -- refresh
  --   - handle new retvals (propagate)
  let e' :: Eternity w
      e' = e { retvals = retvals e ++ [newRetval] }
  -- msp $ ("e", e)
  -- msp $ ("e'", e')
  let h :: History w
      h = startHistory e'
  -- msp $ ("h", h)
  let h' = replayHistory h
  -- msp $ ("h'", h')
  --   - handle new steps (fork)
  handleNewSteps e h'
  let (lastW, e'') = endHistory e' h'
  msp ("wut", length (monitorings e''))
  -- msp $ ("e''", e'')
  -- run monitors
  runMonitors lastW e''
  -- TODO
  -- msp "loop"
  mainLoop e''

runMonitors :: w -> Eternity w -> IO ()
runMonitors w e = forM_ (monitorings e) (runMonitor w)
runMonitor :: w -> Monitoring w -> IO ()
runMonitor w (Monitoring v mon) = do
  let x = rd w v
  mon x

-- TODO not forking yet, running inline
handleNewSteps :: Eternity w -> History w -> IO ()
handleNewSteps e h = do
  let newSteps = drop (nextStep e) (steps h)
      indices = drop (nextStep e) [0..]
      both = zip indices newSteps
  -- msp $ "nextStep " ++ show (nextStep e) ++ " steps " ++ show (length (steps h))
  -- msp $ "running " ++ show (length newSteps) ++ " steps"
  forM_ both $ \(index, Step action _) -> do
    -- msp $ "run step " ++ show index
    wrap index action
  where wrap :: Show a => Int -> IO a -> IO ()
        wrap index action = do
          x <- action
          let retval = Retval index (show x)
          writeChan (retvalChan e) retval

replayHistory :: Show w => History w -> History w
replayHistory h = loop h (hRetvals h)
  where loop :: Show w => History w -> [Retval] -> History w
        loop h [] = h
        loop h (rv:rvs) =
          let -- w = last (ws h)
              Retval index s = rv
              step = vindex "replayHistory" (steps h) index
              prog = applyContinuation step s
              g = startGeneration h
              g' = runProgram g prog
              h' = endGeneration h g'
           in loop h' rvs

sleepRand :: Double -> Double -> IO ()
sleepRand lo hi = do
  duration <- getStdRandom (randomR (lo, hi))
  msp $ "sleeping " ++ show duration
  threadDelay $ floor $ duration * 1_000_000
  msp $ "slept " ++ show duration

sleepAfter :: Double -> Double -> Core w -> Core w
sleepAfter lo hi k = Call (Step (sleepRand lo hi) (\() -> Program [k]))

countDown :: Int -> Core W
countDown (-1) = Done
countDown n = Call (Step (threadDelay 1000000)
                         (\() -> Program [
                                   Call (Step (do msp ("countDown " ++ show n); return (n - 1))
                                        (\n -> Program [countDown n]))]))

mapCall :: Core w -> [a] -> (a -> IO ()) -> Core w
mapCall k [] _ = k
mapCall k (x:xs) corer =
  Call (Step (corer x)
             (\() -> Program [mapCall k xs corer]))

writeAFile :: FilePath -> Int -> IO ()
writeAFile dir n = do
  let ns = show n
  writeFile (dir ++ "/" ++ ns) (ns ++ "\n")

cleanDir :: Core w -> FilePath -> Core w
cleanDir k dir =
  let k' = Call (Step (removeDirectory dir) (\() -> Program [k]))
      remover f = removeFile (dir ++ "/" ++ f)
  in Call (Step (listDirectory dir)
                (\files -> Program [
                  mapCall k' files (\f -> do sleepRand 1 3; remover f)]))

filesThing :: Int -> FilePath -> Core W
filesThing num dir =
  Call (Step (createDirectory dir)
             (\() -> Program [mapCall (cleanDir Done dir) [0..num-1]
                                      (\f -> (do sleepRand 1 3; writeAFile dir f))]))

program :: Program W
program = Program
  [ Mon (Monitoring root monnie)
  , Assign (Write baa 140)
  , filesThing 4 "dirr"
  -- , countDown 5
  -- , Call (Step (do msp "hey" ; return 3)
  --        (\n -> Program [Call (Step (msp $ "hey2 " ++ show n) (\() -> Program [Done]))]))
  -- , Call (Step (listDirectory ".")
  --        (\files -> Program [Call (Step (msp files) (\() -> Program [Done]))]))
  -- , Assign (Write bbb 1111)
  ]

monnie :: W -> IO ()
monnie w = msp $ "MON " ++ show w

logMain = do
  -- msp $ cleanDir Done "dirr"
  startLoop theWorld (\args -> program)
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
