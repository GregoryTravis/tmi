{-# Language GADTs, FlexibleContexts, KindSignatures, NamedFieldPuns, NumericUnderscores,
   QualifiedDo, RecordWildCards, ScopedTypeVariables, TypeApplications #-}

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
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Tuple
-- import Type.Reflection
-- import System.CPUTime (getCPUTime, cpuTimePrecision)
import System.Directory
import System.Environment
import System.IO.Unsafe
import System.Random
import Unsafe.Coerce

import Core
import Ext
import InternalCallRunner
import Lift
import qualified Monad as M
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
-- ==== cleanup
-- + fix segfault
-- + move old tests into a test file
-- + rename test Main
-- + other test warnings
-- + rename stuff in Innards
-- ==== meta
-- - mvar in-flight counter
-- - don't actually need the bool
-- - Meta.hs
-- - tmi cli
-- - run filesThing with it
-- ==== signup
-- - do it
-- ==== more cleanup
-- - simplify execution framework before moving things to modules
-- - mainloop to module
-- - other things to module
-- - fanin tests somewhere (just check that it finishes?)
-- --
-- - dir for dbs and ignore it
-- - sho -> scripts
-- --
-- - Don't like applyContinuation in Log's recon
-- - Don't like that I have VNamed names where they're declared and also in recon
-- - Don't like duplication of VNameds
-- - Tmi re-export module
-- - remove V prefix from V ctors
-- - ooo: <> for R (not for its contents) and then you don't have to say rc c = ... (?)
-- - various lowercase 'q's
-- - rename BS/etc
-- - general renaming
-- ====
-- - inject external response
--   - add one to files thing
-- - web server (or plain socket)
-- - invitation workflow
-- - tests for that
-- - MILESTONE
-- ====
-- - does bplus work if you change it to Num?
-- - multi-module registry
-- - oh shit you need some pragmas etc for unsafePerformIO
--   - https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-IO.html
--   - scroll down a bit

data W = W { aa :: Int, bb :: Int, fanInCount :: Int, fanInCount2 :: Int } deriving (Read, Show)

type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W
bi :: V f -> V r -> Bi f r
bi = Ty.Bi

addEmBi :: Bi (Int -> Int -> Int)
              (Int -> R Int -> Int -> R Int -> R Int)
addEmBi = bi (VNamed "bplus" bplus) (VNamed "bplus_" bplus_)
addEm = lift2 addEmBi

eqBi :: Eq a => Bi (a -> a -> Bool) (a -> R a -> a -> R a -> R Bool)
eqBi = nuni "eq" (==)
eqV :: Eq a => V a -> V a -> V Bool
eqV = lift2 eqBi

bplus :: Int -> Int -> Int
bplus = (+)
bplus_ :: Int -> R Int -> Int -> R Int -> R Int
bplus_ _ ra _ rb = mkR rc
  where rc c = let a = c `div` 2
                   b = c - a
                in write ra a <> write rb b

bFanInCount :: V Int
-- TODO Shouldn't this use a lifter?
bFanInCount = VBiSeal (BiApp (bi (VNamed "fanInCount" fanInCount)
                                 (VNamed "fanInCount_" fanInCount_)) root)
fanInCount_ :: W -> R W -> R Int
fanInCount_ w wr = mkR ir
  where ir fanInCount = write wr $ w { fanInCount }

bFanInCount2 :: V Int
-- TODO Shouldn't this use a lifter?
bFanInCount2 = VBiSeal (BiApp (bi (VNamed "fanInCount2" fanInCount2)
                                 (VNamed "fanInCount2_" fanInCount2_)) root)
fanInCount2_ :: W -> R W -> R Int
fanInCount2_ w wr = mkR ir
  where ir fanInCount2 = write wr $ w { fanInCount2 }

root :: V W
root = VRoot
theWorld :: W
theWorld = W { aa = 13, bb = 100, fanInCount = 112, fanInCount2 = 223 }

recon :: String -> a
recon "bplus" = unsafeCoerce $ VNamed "bplus" bplus
recon "bplus_" = unsafeCoerce $ VNamed "bplus_" bplus_
recon "smallProg" = unsafeCoerce $ VNamed "smallProg" smallProgBlef
recon s = error $ "recon?? " ++ s

sleepRand :: Double -> Double -> IO ()
sleepRand lo hi = do
  duration <- getStdRandom (randomR (lo, hi))
  -- msp $ "sleeping " ++ show duration
  threadDelay $ floor $ duration * 1_000_000
  -- msp $ "slept " ++ show duration

-- sleepAfter :: Double -> Double -> Core w -> Core w
-- sleepAfter lo hi k = Call (InternalCall "yo" (sleepRand lo hi) (\() -> Program [k]))

countDown :: Int -> Core W
countDown (-1) = Done
countDown n = Call (InternalCall "yo" (threadDelay 1000000)
                         (\() -> Program [
                                   Call (InternalCall "yo" (do msp ("countDown " ++ show n); return (n - 1))
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

timeCall :: String -> (Core W -> Core W) -> Core W
timeCall note kjob = Sub (Program
  [ Call (InternalCall "time start" (do t <- getSystemTime; return $ systemToUTCTime t)
    (\start ->
      let k = Call (
                InternalCall "time end" (do t <- getSystemTime; return $ systemToUTCTime t)
                (\end -> let diff = end `diffUTCTime` start
                          in Program [Call (InternalCall "time show" (msp $ "duration " ++ note ++
                                                                            " " ++ show diff)
                                        (\() -> Program [Done]))]))
       in Program [kjob k])) ])

mapCallCPS :: Core w -> [a] -> (a -> IO ()) -> Core w
mapCallCPS k [] _ = k
mapCallCPS k (x:xs) corer =
  Call (InternalCall "mapCallCPS" (corer x)
             (\() -> Program [mapCallCPS k xs corer]))

writeAFile :: FilePath -> Int -> IO ()
writeAFile dir n = do
  let ns = show n
  writeFileExt (dir ++ "/" ++ ns) ("i am " ++ ns ++ "\n")

slp = sleepRand 0.2 0.4
-- slp = sleepRand 2 4

cleanDir :: V Int -> Core W -> FilePath -> Core W
cleanDir counter k dir =
  let k' = Call (InternalCall "removeDirectoryExt" (removeDirectoryExt dir) (\() -> Program [k]))
      remover f = removeFileExt (dir ++ "/" ++ f)
  in Call (InternalCall "listDirectory" (listDirectory dir)
                (\files -> Program [
                  -- mapCallCPS k' files (\f -> do slp; remover f)]))
                  mapCallFanIn counter
                    (flip map files $ \f ->
                      (\k -> Call (InternalCall "slp+remove" (do slp; remover f)
                                        (\() -> Program [k]))))
                    k']))

filesThing :: V Int -> Int -> FilePath -> Core W -> Core W
filesThing counter num dir k =
  Call (InternalCall "createDirectoryExt" (createDirectoryExt dir)
             -- (\() -> Program [mapCallCPS (cleanDir Done dir) [0..num-1]
             --                             (\f -> (do slp; writeAFile dir f))]))
             (\() -> Program [mapCallFanIn counter
                                           (flip map [0..num-1] $ \f ->
                                             (\k -> Call (InternalCall "slp+writeAFile" (do slp; writeAFile dir f)
                                                               (\() -> Program [k]))))
                                           (cleanDir counter k dir)]))
                                           -- Done]))

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

run :: (Read w, Show w) => LookerUpper w -> FilePath -> IO ()
run lookerUpper dbdir = do
  icr <- mkInternalCallRunner
  let loop = do
        ck <- readCK dbdir
        initW <- readInitW dbdir
        -- msp ("ck start", ck)
        let (w', calls) = processEvents lookerUpper initW (eventLog ck)
        -- msp ("initW", initW)
        -- msp ("last w", w')
        -- msp ("processedEvents, got", length calls)
        -- processCalls calls (eventLog ck)
        -- msp ("write to ICR", length calls, (eventLog ck))
        icrWrite icr (calls, eventLog ck)
        -- msp "retval wait"
        r <- icrRead icr
        -- msp $ "retval wait got " ++ show r
        let ck' = ck { eventLog = eventLog ck ++ [r] }
        writeCK dbdir ck'
        -- msp ("ck end", ck')
        loop
        -- return ()
  loop

-- mkInternalCall "yo"Runner :: Chan (Event w) -> IO (Chan ([Call w], [Event w]))

-- -- Get all calls that don't have a retval yet and run them
-- -- no wait
-- processCalls :: [Call w] -> [Event w] -> IO ()
-- processCalls calls events = do
--   return ()

-- Iterate through the event list. Each one produces a Program which gives us a new w
-- and some steps to add to the step list.
-- A new InternalCall "yo" has an IO which only run if the retval list doesn't already have
-- a value (which is a witness for that IO having already been run).
processEvents :: (Show w) => LookerUpper w -> w -> [Event w] -> (w, [Call w])
processEvents lookerUpper w events = processEvents' lookerUpper w events []

processEvents' :: (Show w) => LookerUpper w -> w -> [Event w] -> [Call w] -> (w, [Call w])
processEvents' lookerUpper w [] calls = (w, calls)
processEvents' lookerUpper w (e:es) calls =
  let (w', newCalls) = processEvent lookerUpper w e calls
      calls' = calls ++ newCalls
   in noeesp ("processEvents", (e:es), "old", calls, "new", newCalls, "all", calls') $ processEvents' lookerUpper w' es calls'

processEvent :: (Show w) => LookerUpper w -> w -> Event w -> [Call w] -> (w, [Call w])
processEvent lookerUpper w e calls =
  let prog = eventToProgram lookerUpper e calls
      (w', newCalls) = runProgram w prog
      -- allCalls = calls ++ newCalls
   in noeesp ("processEvent runProgram", e, calls, newCalls) $ (w', newCalls)
   -- in eesp ("processEvent", e) $ runProgram w prog

eventToProgram :: LookerUpper w -> Event w -> [Call w] -> Program w
eventToProgram lookerUpper (Command command) _ = lookerUpper command
eventToProgram lookerUpper r@(Retval index rs) calls =
  let call = vindex "eventToProgram" (noeesp ("eventToProgram", length calls, index, r) calls) index
      program = applyContinuation call rs
   in noeesp ("applyContinuation", r, call, program) $ program

-- data Event w = Retval Int String | Command [String] deriving (Show, Read)

-- TODO do these need IO?
runProgram :: (Show w) => w -> Program w -> (w, [Call w])
runProgram w (Program cores) =
  let (writes, calls) = runCores w cores [] []
      w' = propWrite w (Writes writes)
   in (w', calls)

-- TODO: appends very slow
runCores :: w -> [Core w] -> [Write w] -> [Call w] -> ([Write w], [Call w])
runCores w (core:cores) writes calls =
  let (newWrites, newCalls) = runCore w core
   in runCores w cores (writes ++ newWrites) (calls ++ newCalls)
runCores w [] writes calls = (writes, calls)

runCore :: w -> Core w -> ([Write w], [Call w])
runCore w c = noeesp ("running core", c) $ runCore' w c
-- runCore w c = runCore' w c
runCore' :: w -> Core w -> ([Write w], [Call w])
runCore' w (Assign write) = ([write], [])
runCore' w (Call call) = ([], [call])
runCore' w (Sub (Program cores)) = runCores w cores [] []
runCore' w (Cond vbool th el) =
  let b = rd w vbool
      next = if b then th else el
   in runCore w next
-- runCore' w (Named s c) = runCore w c
runCore' w Done = ([], [])
-- runCore w x = error $ "??? " ++ show x
-- runCore :: w -> Core w -> [Write w] -> [Call w] -> ([Write w], [Call w])
-- runCore w (Assign write) writes calls = (writes ++ [write], calls)
-- runCore w (Call) writes calls = (writes, calls ++ [call])

-- data Core w = Assign (Write w) | Call (Call w) | Sub (Program w)
--             | Cond (V w Bool) (Core w) (Core w) | Done

type LookerUpper w = [String] -> Program w
lookupCommand :: LookerUpper W
lookupCommand ["program", numS] = program (read numS)

tmiMetaMain :: forall w. (Read w, Show w) => Proxy w -> FilePath -> [String] -> IO ()
tmiMetaMain proxy dbdir ["injectRetval", indexS, val] =
  injectEvent dbdir ((Retval (read indexS) val) :: Event w)
tmiMetaMain proxy dbdir ("injectCommand" : command) =
  injectEvent dbdir ((Command command) :: Event w)
-- tmiMetaMain proxy dbdir ["run"] = run dbdir

blef0 :: Blef Int
blef0 = Blef "blef0" (return 12)
a2Blef1 :: Int -> Blef String
a2Blef1 n = Blef "a2Blef1" (do msp ("a2Blef1", n); return $ show (n + 1))
a2Blef2 :: String -> Blef Double
a2Blef2 ns = Blef "a2Blef2" (do msp ("a2Blef2", ns); return $ 1.5 * (read ns))
a2Blef3 :: Double -> Blef Double
a2Blef3 n = Blef "a2Blef3" (do msp ("a2Blef3", n); return $ 2.0 * n)

qq :: Blef Double
-- qq = boond (boond blef0 a2Blef1) a2Blef2
_qq = blef0 M.>>= a2Blef1 M.>>= a2Blef2 M.>>= a2Blef3

__qq = (Blef "blef0" (return 12)) M.>>=
     (\n -> (Blef "a2Blef1" (do msp ("a2Blef1", n); return $ show (n + 1))) M.>>=
            (\ns -> (Blef "a2Blef2" (do msp ("a2Blef1", ns); return $ 1.5 * (read ns))) M.>>=
                    (\n -> (Blef "a2Blef3" (do msp ("a2Blef3", n); return $ 2.0 * n)))))

-- TODO: Perhaps we could leave off M. on the operator if this were in a separate module
-- that was hiding the standard bind?
qq = M.do
  n <- Blef "blef0" (return 12)
  ns <- Blef "a2Blef1" (do msp ("a2Blef1", n); return $ show (n + 1))
  n' <- Blef "a2Blef2" (do msp ("a2Blef2", ns); return $ 1.5 * (read ns))
  n'' <- (Blef "a2Blef3" (do msp ("a2Blef3", n'); return $ 2.0 * n')) M.>>= (\x -> Blef "x" (do msp ("x", x); return $ x + 1))
  M.return (return n'')

-- qq' :: Blef Double
-- qq' = Blefs (Blefs (Blef (return 12)) (\n -> Blef (return $ show (n + 1)))) (\ns -> Blef (return $ 1.5 * (read ns)))
-- -- qq' = Bs (Bs B nB) nB

-- foo :: Program w
-- foo = toProg sdone qq

bsp :: Show a => a -> Blef ()
bsp a = Blef "bsp" (msp a)

io :: IO a -> Blef a
io action = Blef "" action

mapBlef :: (Read b, Show b) => (a -> Blef b) -> [a] -> Blef [b]
mapBlef bf [] = M.return (return [])
mapBlef bf (a:as) = M.do
  b <- bf a
  bs <- mapBlef bf as
  M.return (return (b:bs))

mapBlef_ :: (Read b, Show b) => (a -> Blef b) -> [a] -> Blef ()
mapBlef_ bf as = M.do
  mapBlef bf as
  M.return (return ())

cleanDirSeq :: FilePath -> Blef ()
cleanDirSeq dir = M.do
  files <- Blef "listDirectory" (listDirectory dir)
  Blef "msp" (msp ("files", files))
  let remover f = M.do
        io slp
        Blef "removeFileExt" $ removeFileExt (dir ++ "/" ++ f)
  () <- mapBlef_ remover files
  Blef "removeDirectoryExt" (removeDirectoryExt dir)
  M.return (return ())

filesThingSeq :: Int -> FilePath -> Blef ()
filesThingSeq num dir = M.do
  Blef "createDirectoryExt" (createDirectoryExt dir)
  let createIt n = M.do
        io slp
        if n == 3
          then M.do
            extraN <- EBlef "ftexty" (\h -> msp $ "ft handle " ++ show h)
            Blef "writeAFile" (writeAFile dir (n + extraN))
          else Blef "writeAFile" (writeAFile dir n)
  mapBlef_ createIt [0..num-1]
  cleanDirSeq dir
  M.return (return ())

filesThingProg :: FilePath -> Int -> Program W
filesThingProg dir num = toProg sdone (filesThingSeq num dir)

-- Very ugly proof that a Blef is Nice
smallProg :: Program W
smallProg = toProg sdone (smallProgBlef 133)
-- TODO rid of dummy param
smallProgBlef :: Int -> Blef ()
smallProgBlef _ = M.do
  x <- M.return (return 12)
  Blef "msp" (msp x)
smallProgV = VNamed "smallProg" smallProgBlef
smallProgU = uni smallProgV
smallProgL = lift1 smallProgU
smallProggedV = smallProgL (VNice (144::Int))
smallProggedV' :: Ty.V W (Blef ())
smallProggedV' = head $ tail $ unsafePerformIO (roundTrip recon smallProggedV)
smallProggedRead = rd theWorld smallProggedV'
smallProg' :: Program w
smallProg' = toProg sdone smallProggedRead

-- Fan-in using an mvar; not replayable
-- par :: (Show a, Read a) => [Blef a] -> Blef [a]
-- par blefs = M.do
--   mv <- io $ newMVar []
--   runBlef blef = M.do
--     r <- blef
--     rs <- io $ takeMVar mv
--     io $ putMVar mv (r : rs)
--   flip mapBlef_ blef runBlef
                         
-- par [] = M.return (return [])

exty :: Blef Int
exty = M.do
  n <- EBlef "exty0" (\h -> msp $ "handle " ++ show h)
  io $ msp $ "exty got " ++ show n
  M.return $ return $ n + 1

extyProg :: Program W
extyProg = toProg sdone exty

program :: Int -> Program W
program num = Program
  [
  -- Sub (extyProg)
  -- Sub (smallProg')
  -- timeCall "ayo" (filesThing bFanInCount 10 "dirr")
  Sub (filesThingProg "dirr" 10)
  -- , Sub (filesThingProg "dirr2" 15)

  --Assign (Write baa 140)

  -- Call (InternalCall "yo" (msp "zzzzzzzzzzzzzzzzzzzzzz") (\() -> Program [Done]))
  -- Call (InternalCall "step1"
  --        (do msp "zzzzzzzzzzzzzzzzzzzzzz"; return 12)
  --        (\n -> Program [Call (InternalCall "step2"
  --                                (do msp ("yyyyyyyyyyy", n); return 13)
  --                                (\n -> Program [Done]))]))

  -- , Assign (Write bbb 230)
    -- timeCall (filesThing bFanInCount 10 "dirr")
  -- , filesThing bFanInCount2 160 "dirr2" Done
    -- timeCall (show num) (filesThing bFanInCount num "dirr")
  ]

logMain = do
  msp qq
  msp qq
  msp "----"
  -- works
  let proxy = Proxy :: Proxy W
  let dir = "db"

--   removeDirectoryRecursive "dirr"

  -- Full reset
  let runIt n = do
        removeDbDir dir
        ensureDbDir dir theWorld
        tmiMetaMain proxy dir ["injectCommand", "program", (show n)]
        run lookupCommand dir
  mapM_ runIt [20]
  let continueExty = do
        ensureDbDir dir theWorld
        tmiMetaMain proxy dir ["injectRetval", "8", "43000"]
        run lookupCommand dir
  -- continueExty

  -- tmiMetaMain proxy "db" ["injectRetval", "12", "hey"]

  msp "log hi"
