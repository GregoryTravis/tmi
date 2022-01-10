module MainLoop
( reset
, run
, ensureDbDir
, injectEvent
, App(..)
, AppEnv
) where

import Control.Monad (forM_, when)
import System.Directory

import Core
import InternalCallRunner
import Propagate
import Ty
import Util

type AppEnv w = [String] -> Program w
data App w = App { initialW :: w, appEnv :: AppEnv w }

data Checkpoint w = Checkpoint
  { eventLog :: [Event w] }
  deriving (Show, Read)
emptyCK :: Checkpoint w
emptyCK = Checkpoint { eventLog = [] }

reset :: FilePath -> IO ()
reset dbdir = do
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

injectEvent :: (Show w, Read w) => FilePath -> App w -> Event w -> IO ()
injectEvent dbdir _app e = do
  ck <- readCK dbdir
  -- initW <- readInitW dbdir
  let ck' = ck { eventLog = eventLog ck ++ [e] }
  writeCK dbdir ck'

run :: (Read w, Show w) => App w -> FilePath -> IO ()
run app dbdir = do
  initIcr <- mkInternalCallRunner
  let loop icr = do
        ck <- readCK dbdir
        initW <- readInitW dbdir
        let (w', calls) = processEvents (appEnv app) initW (eventLog ck)
        -- icrWrite icr (calls, eventLog ck)
        icr' <- icrRun icr calls (eventLog ck)
        inf <- icrInFlight icr'
        msp $ "ICR " ++ show inf
        done <- icrDone icr
        if not done
          then do msp "going to read"
                  r <- icrRead icr'
                  let ck' = ck { eventLog = eventLog ck ++ [r] }
                  writeCK dbdir ck'
                  loop icr'
          else do msp "Nothing to do, exiting."
                  return ()
  loop initIcr

-- Iterate through the event list. Each one produces a Program which gives us a new w
-- and some steps to add to the step list.
-- A new InternalCall "yo" has an IO which only run if the retval list doesn't already have
-- a value (which is a witness for that IO having already been run).
processEvents :: (Show w) => AppEnv w -> w -> [Event w] -> (w, [Call w])
processEvents lookerUpper w events = processEvents' lookerUpper w events []

processEvents' :: (Show w) => AppEnv w -> w -> [Event w] -> [Call w] -> (w, [Call w])
processEvents' lookerUpper w [] calls = (w, calls)
processEvents' lookerUpper w (e:es) calls =
  let (w', newCalls) = processEvent lookerUpper w e calls
      calls' = calls ++ newCalls
   in noeesp ("processEvents", (e:es), "old", calls, "new", newCalls, "all", calls') $ processEvents' lookerUpper w' es calls'

processEvent :: (Show w) => AppEnv w -> w -> Event w -> [Call w] -> (w, [Call w])
processEvent lookerUpper w e calls =
  let prog = eventToProgram lookerUpper e calls
      (w', newCalls) = runProgram w prog
      -- allCalls = calls ++ newCalls
   in noeesp ("processEvent runProgram", e, calls, newCalls) $ (w', newCalls)
   -- in eesp ("processEvent", e) $ runProgram w prog

eventToProgram :: AppEnv w -> Event w -> [Call w] -> Program w
eventToProgram lookerUpper (Command command) _ = lookerUpper command
eventToProgram lookerUpper r@(Retval index rs) calls =
  let call = vindex "eventToProgram" (noeesp ("eventToProgram", length calls, index, r) calls) index
      program = applyContinuation call rs
   in noeesp ("applyContinuation", r, call, program) $ program

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
