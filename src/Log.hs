{-# Language GADTs, FlexibleContexts, KindSignatures, NamedFieldPuns, NumericUnderscores,
   RecordWildCards, ScopedTypeVariables, TypeApplications #-}

module Log
( logMain
) where

-- import Data.Dynamic
-- import Data.Kind (Type)
import Control.Concurrent
import Control.Monad (forM_, when)
import Data.Maybe
import Data.Proxy
-- import Data.Traversable (for)
import Data.Tuple
-- import Type.Reflection
import System.Directory
import System.Environment
import System.Random
import Unsafe.Coerce

import Ext
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
--   - move monitors out of eternity
--   - save eternity to checkpoint file in db dir and read on startup
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


data W = W { aa :: Int, bb :: Int, fanInCount :: Int } deriving (Read, Show)

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

eqBi :: Eq a => Bi (a -> a -> Bool) (a -> R a -> a -> R a -> R Bool)
eqBi = nuni "eq" (==)
eqV :: Eq a => V a -> V a -> V Bool
eqV = lift2 eqBi

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

bFanInCount :: V Int
-- TODO Shouldn't this use a lifter?
bFanInCount = VBiSeal (BiApp (bi (VNamed "fanInCount" fanInCount)
                                 (VNamed "fanInCount_" fanInCount_)) root)
fanInCount_ :: W -> R W -> R Int
fanInCount_ w wr = mkR ir
  where ir fanInCount = write wr $ w { fanInCount }

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ r = mkR r'
  where r' i = write r (i - 1)

root :: V W
root = VRoot
theWorld :: W
theWorld = W { aa = 13, bb = 100, fanInCount = 112 }

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

sleepRand :: Double -> Double -> IO ()
sleepRand lo hi = do
  duration <- getStdRandom (randomR (lo, hi))
  msp $ "sleeping " ++ show duration
  threadDelay $ floor $ duration * 1_000_000
  msp $ "slept " ++ show duration

-- sleepAfter :: Double -> Double -> Core w -> Core w
-- sleepAfter lo hi k = Call (InternalCall (sleepRand lo hi) (\() -> Program [k]))

countDown :: Int -> Core W
countDown (-1) = Done
countDown n = Call (InternalCall (threadDelay 1000000)
                         (\() -> Program [
                                   Call (InternalCall (do msp ("countDown " ++ show n); return (n - 1))
                                        (\n -> Program [countDown n]))]))

-- Initializes the counter, runs each cps-based core and when they're all done, runs
-- the main k.
mapCallFanIn :: V Int -> [Core W -> Core W] -> Core W -> Core W
mapCallFanIn counter kjobs k =
  let n = length kjobs
      -- countk :: () -> Core w
      countk = Sub (Program
        [ Assign (VWrite counter (addEm counter (VNice (1::Int))))
        , Cond (eqV counter (VNice (n-1)))
               k
               Done])
      jobs = map ($ countk) kjobs
  in Sub (Program
          [ Assign (Write counter 0)
          , Sub (Program jobs)
          ])
  

mapCallCPS :: Core w -> [a] -> (a -> IO ()) -> Core w
mapCallCPS k [] _ = k
mapCallCPS k (x:xs) corer =
  Call (InternalCall (corer x)
             (\() -> Program [mapCallCPS k xs corer]))

writeAFile :: FilePath -> Int -> IO ()
writeAFile dir n = do
  let ns = show n
  writeFileExt (dir ++ "/" ++ ns) (ns ++ "\n")

slp = sleepRand 2 4

cleanDir :: Core W -> FilePath -> Core W
cleanDir k dir =
  let k' = Call (InternalCall (removeDirectoryExt dir) (\() -> Program [k]))
      remover f = removeFileExt (dir ++ "/" ++ f)
  in Call (InternalCall (listDirectory dir)
                (\files -> Program [
                  -- mapCallCPS k' files (\f -> do slp; remover f)]))
                  mapCallFanIn bFanInCount
                    (flip map files $ \f ->
                      (\k -> Call (InternalCall (do slp; remover f)
                                        (\() -> Program [k]))))
                    k']))

-- mapCallFanIn :: V Int -> [Core W -> Core W] -> Core W -> Core W

filesThing :: Int -> FilePath -> Core W
filesThing num dir =
  Call (InternalCall (createDirectoryExt dir)
             -- (\() -> Program [mapCallCPS (cleanDir Done dir) [0..num-1]
             --                             (\f -> (do slp; writeAFile dir f))]))
             (\() -> Program [mapCallFanIn bFanInCount
                                           (flip map [0..num-1] $ \f ->
                                             (\k -> Call (InternalCall (do slp; writeAFile dir f)
                                                               (\() -> Program [k]))))
                                           (cleanDir Done dir)]))

program :: Program W
program = Program
  [ Assign (Write baa 140)
  , filesThing 40 "dirr"
  ]

monnie :: W -> IO ()
monnie w = msp $ "MON " ++ show w

data Checkpoint w = Checkpoint
  { eventLog :: [Event w] }
  deriving (Show, Read)
emptyCK :: Checkpoint w
emptyCK = Checkpoint { eventLog = [] }

removeDbDir :: FilePath -> IO ()
removeDbDir dbdir = do
  de <- doesDirectoryExist dbdir
  when de $ removeDirectoryRecursive dbdir

-- Create the db with empty checkpoint if it doesn't exist.
ensureDbDir :: Show w => FilePath -> w -> IO ()
ensureDbDir dbdir initW = do
  de <- doesDirectoryExist dbdir
  if not de
    then do
      let initWFile = dbdir ++ "/initW"
          initCKFile = dbdir ++ "/ck"
      createDirectory dbdir
      writeFile initWFile (show initW)
      writeFile initCKFile (show emptyCK)
    else return ()

readDbFile :: Read a => FilePath -> FilePath -> IO a
readDbFile dbdir file = do
  s <- readFile' (dbdir ++ "/" ++ file)
  return $ read s
readCK :: (Show w, Read w) => FilePath -> IO (Checkpoint w)
readCK dbdir = readDbFile dbdir "ck"
readInitW :: (Show w, Read w) => FilePath -> IO w
readInitW dbdir = readDbFile dbdir "initW"
writeCK :: (Show w, Read w) => FilePath -> Checkpoint w -> IO ()
writeCK dbdir ck = writeFile (dbdir ++ "/ck") (show ck)

injectEvent :: (Show w, Read w) => FilePath -> Event w -> IO ()
injectEvent dbdir e = do
  ck <- readCK dbdir
  -- initW <- readInitW dbdir
  let ck' = ck { eventLog = eventLog ck ++ [e] }
  writeCK dbdir ck'

run :: (Read w, Show w) => LookerUpper w -> Proxy w -> FilePath -> IO ()
run lookerUpper proxy dbdir = do
  ck <- readCK dbdir
  initW <- readInitW dbdir
  let (w', calls) = processEvents lookerUpper initW (eventLog ck)
  msp "before"
  msp initW
  msp "after"
  msp w'
  processCalls calls (eventLog ck)
  return ()

-- Get all calls that don't have a retval yet and run them
-- no wait
processCalls :: [Call w] -> [Event w] -> IO ()
processCalls calls events = do
  return ()

-- Iterate through the event list. Each one produces a Program which gives us a new w
-- and some steps to add to the step list.
-- A new InternalCall has an IO which only run if the retval list doesn't already have
-- a value (which is a witness for that IO having already been run).
processEvents :: (Show w) => LookerUpper w -> w -> [Event w] -> (w, [Call w])
processEvents lookerUpper w events = processEvents' lookerUpper w events []

processEvents' :: (Show w) => LookerUpper w -> w -> [Event w] -> [Call w] -> (w, [Call w])
processEvents' lookerUpper w [] calls = (w, calls)
processEvents' lookerUpper w (e:es) calls = do
  let (w', calls') = processEvent lookerUpper w e calls
  processEvents' lookerUpper w' es calls'

processEvent :: (Show w) => LookerUpper w -> w -> Event w -> [Call w] -> (w, [Call w])
processEvent lookerUpper w e calls = do
  let prog = eventToProgram lookerUpper e calls
   in runProgram w prog

eventToProgram :: LookerUpper w -> Event w -> [Call w] -> Program w
eventToProgram lookerUpper (Command command) _ = lookerUpper command
eventToProgram lookerUpper (Retval index rs) calls =
  let call = vindex "eventToProgram" calls index
   in applyContinuation call rs

-- data Event w = Retval Int String | Command [String] deriving (Show, Read)

-- TODO do these need IO?
runProgram :: (Show w) => w -> Program w -> (w, [Call w])
runProgram w (Program cores) =
  let (writes, calls) = runCores w cores [] []
      w' = propToRoot w (Writes writes)
   in (w', calls)

runCores :: w -> [Core w] -> [Write w] -> [Call w] -> ([Write w], [Call w])
runCores w (core:cores) writes calls =
  let (newWrites, newCalls) = runCore w core
   in runCores w cores (writes ++ newWrites) (calls ++ newCalls)
runCores w [] writes calls = (writes, calls)

runCore :: w -> Core w -> ([Write w], [Call w])
runCore w (Assign write) = ([write], [])
runCore w (Call call) = ([], [call])
-- runCore :: w -> Core w -> [Write w] -> [Call w] -> ([Write w], [Call w])
-- runCore w (Assign write) writes calls = (writes ++ [write], calls)
-- runCore w (Call) writes calls = (writes, calls ++ [call])

-- data Core w = Assign (Write w) | Call (Call w) | Sub (Program w)
--             | Cond (V w Bool) (Core w) (Core w) | Done

type LookerUpper w = [String] -> Program w
lookupCommand :: LookerUpper W
lookupCommand ["program"] = program

tmiMetaMain :: forall w. (Read w, Show w) => Proxy w -> FilePath -> [String] -> IO ()
tmiMetaMain proxy dbdir ["injectRetval", indexS, val] =
  injectEvent dbdir ((Retval (read indexS) val) :: Event w)
tmiMetaMain proxy dbdir ("injectCommand" : command) =
  injectEvent dbdir ((Command command) :: Event w)
-- tmiMetaMain proxy dbdir ["run"] = run proxy dbdir

logMain = do
  let dir = "db"
  -- removeDbDir dir
  let proxy = Proxy :: Proxy W
  ensureDbDir dir theWorld
  run lookupCommand proxy dir
  -- tmiMetaMain "db" ["injectRetval", "12", "hey"]
  -- tmiMetaMain "db" ["injectCommand", "program"]

  -- -- msp $ cleanDir Done "dirr"
  -- startLoop theWorld (\args -> program)
  -- -- finalWorld <- runProgram theWorld program
  -- -- msp finalWorld

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
