{-# Language GADTs, FlexibleContexts, KindSignatures, NamedFieldPuns, NumericUnderscores,
   QualifiedDo, RecordWildCards, ScopedTypeVariables, TypeApplications #-}

module Log
( logApp
, logMain
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

import Alloc
import Core
import Ext
import Lift
import MainLoop
import qualified Monad as M
import Parr
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
-- ==== parr
-- + parr
-- + BReturn so we can return a V without it being an IO
-- + Allocate pairs
-- - parrList
-- - filesThingPar
-- - test
--   - get a return value from a program?
--   - easier way to run a blef real quick: just the blef, init, and recon
-- ==== meta
-- - mvar in-flight counter
--   + write
--   + don't actually need the bool
--   + check the counter
-- - Meta.hs
--   - rename lookerUpper
-- - tmi cli
-- - run filesThing with it
-- ==== more cleanup
-- - Cond is redundant
-- - why that nested return thing, probably 'rit' is the prob
-- ==== signup
-- - do it
-- ==== more cleanup
-- - recons from different modules, maybe a registry?
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

data W = W { aa :: Int, bb :: Int, fanInCount :: Int, fanInCount2 :: Int
  , pairAllocator :: Alloc (Maybe Int, Maybe String)
  } deriving (Read, Show)

vpairAllocator :: V (Alloc (Maybe Int, Maybe String))
vpairAllocator = VBiSeal (BiApp (bi (VNamed "pairAllocator" pairAllocator)
                                    (VNamed "pairAllocator_" pairAllocator_)) root)
pairAllocator_ :: W -> R W -> R (Alloc (Maybe Int, Maybe String))
pairAllocator_ w wr = mkR ir
  where ir pairAllocator = write wr $ w { pairAllocator }

type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W

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
theWorld = W { aa = 13, bb = 100, fanInCount = 112, fanInCount2 = 223
  , pairAllocator = mkAlloc
  }

recon :: String -> a
recon "bplus" = unsafeCoerce $ VNamed "bplus" bplus
recon "bplus_" = unsafeCoerce $ VNamed "bplus_" bplus_
recon "pairAllocator" = unsafeCoerce $ VNamed "pairAllocator" pairAllocator
recon "pairAllocator_" = unsafeCoerce $ VNamed "pairAllocator_" pairAllocator_
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

-- countDown :: Int -> Core W
-- countDown (-1) = Done
-- countDown n = Call (InternalCall "yo" (threadDelay 1000000)
--                          (\() -> Program [
--                                    Call (InternalCall "yo" (do msp ("countDown " ++ show n); return (n - 1))
--                                         (\n -> Program [countDown n]))]))

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

filesThing' :: V Int -> Int -> FilePath -> Core W -> Core W
filesThing' counter num dir k =
  Call (InternalCall "createDirectoryExt" (createDirectoryExt dir)
             -- (\() -> Program [mapCallCPS (cleanDir Done dir) [0..num-1]
             --                             (\f -> (do slp; writeAFile dir f))]))
             (\() -> Program [mapCallFanIn counter
                                           (flip map [0..num-1] $ \f ->
                                             (\k -> Call (InternalCall "slp+writeAFile" (do slp; writeAFile dir f)
                                                               (\() -> Program [k]))))
                                           (cleanDir counter k dir)]))
                                           -- Done]))

-- tmiMetaMain :: forall w. (Read w, Show w) => Proxy w -> FilePath -> [String] -> IO ()
-- tmiMetaMain proxy dbdir ["injectRetval", indexS, val] =
--   injectEvent dbdir ((Retval (read indexS) val) :: Event w)
-- tmiMetaMain proxy dbdir ("injectCommand" : command) =
--   injectEvent dbdir ((Command command) :: Event w)
-- -- tmiMetaMain proxy dbdir ["run"] = run dbdir

blef0 :: Blef W Int
blef0 = Blef "blef0" (return 12)
a2Blef1 :: Int -> Blef W String
a2Blef1 n = Blef "a2Blef1" (do msp ("a2Blef1", n); return $ show (n + 1))
a2Blef2 :: String -> Blef W Double
a2Blef2 ns = Blef "a2Blef2" (do msp ("a2Blef2", ns); return $ 1.5 * (read ns))
a2Blef3 :: Double -> Blef W Double
a2Blef3 n = Blef "a2Blef3" (do msp ("a2Blef3", n); return $ 2.0 * n)

qq :: Blef W Double
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
  M.return n''

-- qq' :: Blef Double
-- qq' = Blefs (Blefs (Blef (return 12)) (\n -> Blef (return $ show (n + 1)))) (\ns -> Blef (return $ 1.5 * (read ns)))
-- -- qq' = Bs (Bs B nB) nB

-- foo :: Program w
-- foo = toProg sdone qq

bsp :: Show a => a -> Blef w ()
bsp a = Blef "bsp" (msp a)

io :: (Read a, Show a) => IO a -> Blef w a
io action = Blef "" action

mapBlef :: (Read b, Show b) => (a -> Blef w b) -> [a] -> Blef w [b]
mapBlef bf [] = M.return []
mapBlef bf (a:as) = M.do
  b <- bf a
  bs <- mapBlef bf as
  M.return (b:bs)

mapBlef_ :: (Read b, Show b) => (a -> Blef w b) -> [a] -> Blef w ()
mapBlef_ bf as = M.do
  mapBlef bf as
  M.return ()

cleanDirSeq :: FilePath -> Blef w ()
cleanDirSeq dir = M.do
  files <- Blef "listDirectory" (listDirectory dir)
  Blef "msp" (msp ("files", files))
  let remover f = M.do
        io slp
        Blef "removeFileExt" $ removeFileExt (dir ++ "/" ++ f)
  () <- mapBlef_ remover files
  Blef "removeDirectoryExt" (removeDirectoryExt dir)
  M.return ()

filesThingSeq :: Int -> FilePath -> Blef w ()
filesThingSeq num dir = M.do
  Blef "createDirectoryExt" (createDirectoryExt dir)
  let createIt n = M.do
        io slp
        if n == 3 && False
          then M.do
            extraN <- EBlef "ftexty" (\h -> msp $ "ft handle " ++ show h)
            Blef "writeAFile" (writeAFile dir (n + extraN))
          else Blef "writeAFile" (writeAFile dir n)
  mapBlef_ createIt [0..num-1]
  cleanDirSeq dir
  M.return ()

-- Very ugly proof that a Blef is Nice
smallProg :: Program W
smallProg = toProg sdone (smallProgBlef 133)
-- TODO rid of dummy param
smallProgBlef :: Int -> Blef w ()
smallProgBlef _ = M.do
  x <- M.return 12
  Blef "msp" (msp x)
smallProgV = VNamed "smallProg" smallProgBlef
smallProgU = uni smallProgV
smallProgL = lift1 smallProgU
smallProggedV = smallProgL (VNice (144::Int))
smallProggedV' :: Ty.V W (Blef w ())
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

exty :: Blef w Int
exty = M.do
  n <- EBlef "exty0" (\h -> msp $ "handle " ++ show h)
  io $ msp $ "exty got " ++ show n
  M.return $ n + 1

extyProg :: Program W
extyProg = toProg sdone exty

lookupCommand :: AppEnv W
lookupCommand ["filesThing", dir, numS] = filesThing dir (read numS)
lookupCommand ["filesThings", dir, numS, dir2, numS2] = filesThings dir (read numS) dir2 (read numS2)
lookupCommand ["exty"] = extyProg
lookupCommand ["filesThingPar"] = filesThingPar

filesThing :: FilePath -> Int -> Program W
filesThing dir num = toProg sdone (filesThingSeq num dir)

filesThings :: FilePath -> Int -> FilePath -> Int -> Program W
filesThings dir num dir2 num2 = Program
  [ Sub (toProg sdone (filesThingSeq num dir))
  , Sub (toProg sdone (filesThingSeq num2 dir2)) ]

countDown :: String -> Int -> Blef w ()
countDown tag 0 = M.return ()
countDown tag n = M.do
  io $ msp $ "countdown " ++ tag ++ " " ++ show n
  io $ slp -- sleepRand 0.6 1.0
  countDown tag (n - 1)

  -- Can't figure it out
  -- -- n <- Blef "" (return 12)
  -- -- n <- BCallCC (\k -> k 40)
  -- -- n <- BCallCC (\k -> Blefs thing (\n -> k n))
  -- n <- BCallCC (\k -> Blef "" (msp "ignored")) -- (\k -> ...) :: (Int -> Blef String) -> Blef ()
  --                                              -- k :: Int -> Blef String
  --                                              -- BCallCC _ :: Blef ()
  -- io $ msp $ "n " ++ show n
  -- pretend: return ""

filesThingPar :: Program W
filesThingPar = toProg done $ M.do
  -- parr example
  -- all <- BRead vpairAllocator
  let blef0 = M.do io slp
                   M.return 1
      blef1 = M.do io slp
                   M.return "asdf"
  (i, s) <- parr vpairAllocator blef0 blef1
  io $ msp ("holy shit", i, s)

  -- all this works fine
  -- BFork (countDown "aaa" 3)
  -- BFork (countDown "bbb" 4)

  -- works
  -- n <- Blef "" (return 12)

  -- works
  -- n <- BCallCC (\k -> k 40)

  -- works
  -- n <- BCallCC (\k -> M.do io $ msp "omg"

  -- works
  -- let q = 20
  -- n <- BCallCC (\k -> M.do io $ msp "omg"
  --                          if q == 20
  --                            then k 41
  --                            else k 42)

  -- works??
  -- n <- BCallCC (\k -> Blef "" (msp "ignored")) -- (\k -> ...) :: (Int -> Blef String) -> Blef ()
  --                                              -- k :: Int -> Blef String
  --                                              -- BCallCC _ :: Blef ()

  -- io $ msp $ "n " ++ show n
  io $ msp "hi filesThingPar"

-- BCallCC :: ((b -> Blef a) -> Blef c) -> Blef c

logApp = App { initialW = theWorld, appEnv = lookupCommand }

justRun :: (Read w, Show w) => FilePath -> App w -> [String] -> IO ()
justRun dbdir app command = do
  reset dbdir
  ensureDbDir dbdir (initialW app)
  injectEvent dbdir app (Command command)
  run app dbdir

logMain :: IO ()
logMain = justRun "db" logApp ["filesThingPar"]

--program num = Program
--  [
--  -- Sub (extyProg)
--  -- Sub (smallProg')
--  -- timeCall "ayo" (filesThing' bFanInCount 10 "dirr")
--  Sub (filesThingProg "dirr" 10)
--  , Sub (filesThingProg "dirr2" 15)

--  --Assign (Write baa 140)

--  -- Call (InternalCall "yo" (msp "zzzzzzzzzzzzzzzzzzzzzz") (\() -> Program [Done]))
--  -- Call (InternalCall "step1"
--  --        (do msp "zzzzzzzzzzzzzzzzzzzzzz"; return 12)
--  --        (\n -> Program [Call (InternalCall "step2"
--  --                                (do msp ("yyyyyyyyyyy", n); return 13)
--  --                                (\n -> Program [Done]))]))

--  -- , Assign (Write bbb 230)
--    -- timeCall (filesThing bFanInCount 10 "dirr")
--  -- , filesThing bFanInCount2 160 "dirr2" Done
--    -- timeCall (show num) (filesThing bFanInCount num "dirr")
--  ]

-- logMain = do
--   msp qq
--   msp qq
--   msp "----"
--   -- works
--   let proxy = Proxy :: Proxy W
--   let dir = "db"

-- --   removeDirectoryRecursive "dirr"

--   -- Full reset
--   let runIt n = do
--         removeDbDir dir
--         ensureDbDir dir theWorld
--         tmiMetaMain proxy dir ["injectCommand", "program", (show n)]
--         run lookupCommand dir
--   mapM_ runIt [20]
--   let continueExty = do
--         ensureDbDir dir theWorld
--         tmiMetaMain proxy dir ["injectRetval", "17", "53000"]
--         run lookupCommand dir
--   -- continueExty

--   -- tmiMetaMain proxy "db" ["injectRetval", "12", "hey"]

--   msp "log hi"
