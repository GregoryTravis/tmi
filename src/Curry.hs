{-# LANGUAGE ExistentialQuantification,
             GADTs,
             NamedFieldPuns,
             RecordWildCards,
             StandaloneDeriving #-}

module Curry
( R(..)
, Write(..)
, Write1(..)
, emptyWrite
, V(..)
, W(..)
, _db
, _rpc
, Receiver(..)
, hybrid1
, hybrid2
, hybrid3
, (<--)
, (<**>)
, (<$$>)
, History(..)
  -- TODO don't export this
, mkHistory
, getRoot
, TMI
, ExecState(listeners) -- TODO remove, for debugging
, StepState(execState) -- TODO remove, for debugging
-- , tmiRun
, tmiMain
-- , tmiRunIO
-- , persistentTmiRun
-- , writeHistory -- TODO remove, only for setting up
, (<---)
, listen
-- TODO remove after moving listeners out of history
-- , runListeners
, uniqueId
, mkFielder
) where

-- import Control.Concurrent.Chan
import Control.Monad.Cont
import Control.Monad.State.Lazy hiding (execState)
import Data.Dynamic
import Data.Maybe
import Data.Proxy
import Unsafe.Coerce

import ExecId
import Rpc
import UniqueId
import Util

data W d = W
  { db :: d
  , rpc :: Rpc }
  deriving Show
_db = mkFielder "_db" db $ \w a -> w { db = a }
_rpc = mkFielder "_rpc" rpc $ \w a -> w { rpc = a }

-- data Write1 = forall a. Write1 a
data Write1 = forall a. Show a => Write1 (V a) a
deriving instance Show Write1
data Write = Write [Write1] deriving Show
emptyWrite :: Write
emptyWrite = Write []
instance Semigroup Write where
  Write ws <> Write ws' = Write $ ws ++ ws'
data Receiver a = Receiver String (a -> Write)
instance Show (Receiver a) where
  show (Receiver s _) = "REC Receiver " ++ s
-- data Receiver a = Receiver (V a)
data R a = R a (Receiver a)
  deriving Show
infix 1 <--
(<--) :: Receiver a -> a -> Write
Receiver s r <-- x = {-eeesp ("REC <-- call", s) $-} r x
-- Receiver va <-- x = Write [Write1 va x]
-- Receiver va <-- x = Write [Write1 va x]

hybrid1 :: (a -> b) -> (R a -> b -> Write) -> (R a -> R b)
hybrid1 f r ra@(R x rx) = R x' rx'
  where x' = f x
        rx' = Receiver "hybrid1" $ \x -> r ra x

hybrid2 :: (a -> b -> c) -> (R a -> R b -> c -> Write) -> (R a -> R b -> R c)
hybrid2 f r ra@(R x rx) rb@(R y ry) = R z rz
  where z = f x y
        rz = Receiver "hybrid2" $ \x -> r ra rb x

hybrid3 :: (a -> b -> c -> d) -> (R a -> R b -> R c -> d -> Write) -> (R a -> R b -> R c -> R d)
hybrid3 f r ra@(R x rx) rb@(R y ry) rc@(R z rz) = R w rw
  where w = f x y z
        rw = Receiver "hybrid2" $ \x -> r ra rb rc x

data V a where
  VRoot :: V a
  VConst :: (Show a) => String -> a -> V a
  VCheckConst :: (Show a, Eq a) => String -> a -> V a
  VPartialApp :: (Show a) => V (R a -> rest) -> V a -> V rest
  VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
  VApp :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
  VSeal :: (Show a) => V (R a) -> V a
  -- VUnSeal :: (Show a) => V a -> V (R a)

-- more succinct
k :: (Show a) => String -> a -> V a
k = VConst
infixl 4 <**>
(<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
(<**>) = VPartialApp
infixl 4 <$$>
(<$$>) :: (Show a, Show b) => V (R b -> R a) -> V b -> V a
(<$$>) = VApp
-- infixl 4 <$$$>
-- (<$$$>) :: (Show a, Show b) => V (R a -> R b -> R c) -> V a -> V (R b -> R c)
-- (<$$$>) = undefined

-- app2 :: V (R a -> R b -> R c) -> V a -> V (R b -> R c)
-- partialApp :: V (R a -> rest) -> V a -> V rest
-- partialApp = undefined

instance Show (a -> b) where
  show _ = "fn"

instance Show a => Show (V a) where
  show VRoot = "[root]"
  show (VConst s a) = "(VConst " ++ s ++ " " ++ show a ++ ")"
  -- TODO: Replace VConst with this, and add Eq to everything
  show (VCheckConst s a) = "(VCheckConst " ++ s ++ " " ++ show a ++ ")"
  -- show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ (show vfb) ++ ")"
  -- show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ (show va) ++ ")"
  show (VApp vfba vfb) = "(" ++ (show vfba) ++ " " ++ "arg" ++ ")"
  show (VPartialApp vf va) = "(" ++ (show vf) ++ " " ++ "arg" ++ ")"
  show (VSeal va) = "(seal " ++ (show va) ++ ")"

-- sinfulCast :: a -> b
-- sinfulCast = fromJust . fromDynamic . toDyn

-- Stored in reverse order
data History w = History [w]
-- TODO Foldable
histlen :: History w -> Int
histlen (History ws) = length ws

instance Show w => Show (History w) where
  show (History ws) = show ws

mkHistory :: w -> History w
mkHistory w = History [w]

latestState :: History w -> w
latestState (History []) = error "latestState:: empty"
latestState (History (w:_)) = w

getRoot :: History w -> V w
getRoot (History (w:_)) = VRoot

newGeneration :: History w -> w -> History w
newGeneration (History ws) w = History (w:ws)

-- addListener :: History w -> Listener -> History w
-- addListener (History ws listeners) listener =
--   History ws (listener:listeners)

runListeners :: ExecState w -> IO ()
runListeners es = mapM_ runListener (listeners es)
  where runListener (Listener va action) = do
          let a = r (history es) va
          action a

toString :: (Show w) => History w -> String
toString (History ws) = show ws
fromString :: (Read w) => String -> History w
fromString s = History (read s)

r :: History w -> V a -> a
-- r :: W -> V a -> a
r (History (w:_)) VRoot = unsafeCoerce w
r _ (VConst _ x) = x
r _ (VCheckConst _ x) = x
r h (VApp vfbfa vb) = r h (VSeal (VPartialApp vfbfa vb))
-- TODO not crazy about constructing receivers here
-- r w (VApp vf va) = b
--   where f = r w vf
--         a = r w va
--         -- rb = R b (Receiver $ \b' -> Write [Write1 b'])
--         ra = R a (Receiver "r VApp" $ \a' -> Write [Write1 va a'])
--         rb = f ra
--         b = case rb of R b _ -> b
r h (VSeal vra) = a
  where ra = r h vra
        a = case ra of R a _ -> a
r h (VPartialApp vf va) = paf
  where f = r h vf
        a = r h va
        ra = R a (Receiver "r VPartialApp" $ \a' -> Write [Write1 va a'])
        paf = f ra
-- VUnPartialApp :: (Show a) => (V a -> V rest) -> V (R a -> rest)
-- must return (R a -> rest)
r h (VUnPartialApp vvf) = \ra -> ((r h (vvf (VSeal (VConst "uh" ra)))))
-- r h (VUnPartialApp vvf) = \ra ->     -- ra = -- :: rest
--   let va = VConst "VUnPartialApp" ra
--       vRest = vvf va
--       rest = r h vRest
--    in rest

wr :: History w -> V a -> a -> Write
-- wr :: W -> V a -> a -> Write
-- wr w VRoot _ = undefined "Can't write to root"
wr h v@(VConst s _) _ = error $ "Can't write to a const: " ++ s ++ " " ++ (show v)
wr h v@(VCheckConst s x) x'
  | x == x' = emptyWrite
  | otherwise = error $ "VCheckConst: unequal: " ++ show v ++ " <-- " ++ show x'
-- This was just to ignore what I figured was a equi-const write
-- wr h (VConst s _) _ = emptyWrite
-- Can't
-- wr h (VConst s x) x'
--   | x == x' = error "but ok"
--   | otherwise = error $ "Can't write to a const: " ++ s
wr h (VApp vfbfa vb) b = wr h (VSeal (VPartialApp vfbfa vb)) b
  --where -- write = Write [Write1 vb b']
  --      write = reca a
  --      rbra = r w vfbfa
  --      -- ra = rbra rb
  --      R _ (Receiver reca) = rbra rb
  --      rb = R b (Receiver $ \b' -> Write [Write1 vb b'])
  --      b = r w vb
  --      --b' = undefined
-- Good gravy why is this not needed?
wr h (VPartialApp vf va) _ = error "Why is this error not happening"
wr h (VSeal vra) a = write
  where write = {-eeesp ("REC wr2", s) $-} reca a
        R _ (Receiver s reca) = ra
        ra = r h vra

-- Rewrite!

-- State carried through an entire TMI program.
data ExecState w = ExecState
  { execId :: ExecId
  , listeners :: [Listener]
  , history :: History w
  }

-- launchIOs :: [IO ()] -> IO ()
-- launchIOs ios = do

updateRpc :: ExecState (W ww) -> IO (ExecState (W ww))
updateRpc es@(ExecState {..}) = do
  let w = latestState history
      --Rpc { rpc } = rpc w
  rpc' <- refreshRpcs execId (rpc w)
  let w' = w { rpc = rpc' }
      h' = newGeneration h' w'
      es' = es { history = h' }
  return es

-- Runs the action, commits the change, and then listens to the event stream?
tmiMain :: Show db => IO (History (W db)) -> TMI (W db) () -> IO ()
tmiMain hio action = do
  h <- hio
  -- ch <- (newChan :: Chan TMI w ())
  eid <- currentExecId
  let es = ExecState { execId = eid, listeners = [], history = h }
  ((), es') <- runStateT (runTMI action) es
  es'' <- updateRpc es'
  runListeners es''
  return ()

--runStateT :: s -> m (a, s)

tmiMainFile :: FilePath -> TMI w () -> IO ()
tmiMainFile = undefined -- calls tmiMain
-- etc

data StepState w = StepState
  { execState :: ExecState w
  , writes :: Write
  , serial :: Int
  }

type TMIE w a = StateT (ExecState w) IO a
type TMI w a = StateT (StepState w) IO a
-- runStateT :: s -> m (a, s)	

-- TODO: this should be in another state thread?
-- Initialiaze a StepState, run the action, apply the writes, commit the new
-- world to history.
-- runTMI :: ExecState w -> TMI w () -> ExecState w
-- runTMI es action = do
--   let stepState = StepState { execState = es, writes = emptyWrite, serial = 0 }
--   ((), stepState') <- runStateT action stepState
--   let history' = propagateFully (history (execState es)) (writes stepState')
--   let es' = es { history = history' }
--   return es'

runTMI :: Show w => TMI w () -> TMIE w ()
-- runTMI :: TMI w () -> IO ()
runTMI action = do
  es <- get
  let stepState = StepState { execState = es, writes = emptyWrite, serial = 0 }
  ((), stepState') <- liftIO $ runStateT action stepState
  let es' = execState stepState'
  let history' = propagateFully (history es') (writes stepState')
  let es'' = es' { history = history' }
  put es''

listen :: V a -> (a -> IO ()) -> TMI w ()
listen va ioAction = do
  stepState <- get
  let ls = listeners (execState stepState)
  let listener = Listener va ioAction
      ls' = ls ++ [listener]
      stepState' = stepState { execState = (execState stepState) { listeners = ls' } }
  put stepState'

uniqueId :: TMI w UniqueId
uniqueId = do
  ss <- get
  genId <- getGenId
  let nextSerial = serial ss
      ss' = ss { serial = nextSerial + 1 }
  put ss'
  return $ UniqueId (genId, nextSerial)

getGenId :: TMI w Int
getGenId = do
  ss <- get
  return $ histlen (history (execState ss))

getExecId :: TMI w ExecId
getExecId = do
  ss <- get
  return $ execId (execState ss)

{-
-- Monad!

data TmiState w = TmiState
  { writes :: Write
  , execId :: ExecId
  , listeners :: [Listener]
  , history :: History w
  }
initTmiState :: History w -> IO (TmiState w)
initTmiState h = do
  eid <- currentExecId
  return $ TmiState { writes = emptyWrite, execId = eid, listeners = [], history = h }

-- TODO do we need to return a value? Or can 'a' be ()?
type TMI w a = StateT (TmiState w) IO a

tmiRun :: Show w => History w -> TMI w a -> IO (a, History w)
tmiRun h action = do
  ts <- initTmiState h
  let beforeLength = case (history ts) of History ws -> length ws
  (a, ts') <- runStateT action ts
  let afterLength = case (history ts') of History ws -> length ws
  massert "history changed by action" (beforeLength == afterLength)
  msp ("asdf", histlen $ history ts, histlen $ history ts', length $ listeners ts,
        length $ listeners ts')
  let h' = propagateFully (history ts') (writes ts')
      ts'' = ts' { history = h' }
  -- Is liftIO needed here?
  liftIO $ runListeners ts''
  return (a, h')
-}

-- -- This is for implementing runtime features, eg rpc
-- tmiRunIO :: History w -> (w -> IO w) -> IO (History w)
-- tmiRunIO h action = do
--   let w = latestState h
--   w' <- action w
--   return $ newGeneration h w'

propagateFully :: Show w => History w -> Write -> History w
-- propagateFully = propagateOneAtATime
propagateFully = propagateAndApplySequentially

-- Propagate writes one at a time, all the way to the root. So it's sequential.
propagateOneAtATime :: History w -> Write -> History w
propagateOneAtATime h (Write (w:ws)) =
  let w' = propagateOne h first
      h' = newGeneration h w'
      first = Write [w]
      rest = Write ws
   in propagateOneAtATime h' rest
propagateOneAtATime h (Write []) = h

propagateOne :: History w -> Write -> w
propagateOne h (Write []) = r h VRoot
propagateOne h (Write [Write1 VRoot w]) = unsafeCoerce w
propagateOne h (Write [Write1 va a]) =
  let write' = wr h va a
   in propagateOne h write'
propagateOne h (Write writes) = error ("Non-singular write (" ++ (show writes) ++ ")")

propagateBranching :: History w -> Write -> [w]
propagateBranching h (Write []) = []
propagateBranching h (Write [Write1 VRoot w]) = unsafeCoerce w
propagateBranching h (Write [Write1 va a]) =
  let write' = wr h va a
   in propagateBranching h write'
propagateBranching h (Write writes) = mconcat (map applyOne writes)
  where -- applyOne :: Write1 -> [w]
        applyOne (Write1 va a) =
          let write' = wr h va a
           in propagateBranching h write'

-- Propagate the first write in the list, and repeat until it's a root write.
-- Then advance the history with the new world, and continue with the rest.
-- This is very incorrect.
propagateAndApplySequentially :: History w -> Write -> History w
propagateAndApplySequentially h (Write []) = h
propagateAndApplySequentially h (Write (Write1 VRoot w : writes)) =
  let w' = unsafeCoerce w
      h' = newGeneration h w'
   in propagateAndApplySequentially h' (Write writes)
propagateAndApplySequentially h (Write (Write1 va a : writes)) =
  let write' = wr h va a
      writes' = write' <> Write writes
   in propagateAndApplySequentially h writes'

-- propagate branching, log the resulting worlds, and return the original
-- this failed immediately, e.g. map over different length list
propagateNonSingularAndIgnore :: Show w => History w -> Write -> History w
propagateNonSingularAndIgnore h write =
  let ws = propagateBranching h write
   in eeesp ("propagateNonSingularAndIgnore", ws) h

-- -- Propagate a write to as many worlds as it wants, then return the original state.
-- -- Log the many worlds.
-- propagateFreelyThenDrop :: History w -> Write -> History w
-- propagateFreelyThenDrop h write =
--   let worlds = propagateToMany h write
--    in eesp ("worlds", worlds) h

-- propagateToMany :: History w -> Write -> [w]
-- propagateToMany h (Write [Write1 VRoot w]) = [unsafeCoerce w]
-- propagateToMany h (Write wrs) = mconcat (map (propagateToMany . Write . applyOne) wrs)
--   where applyOne :: Write1 -> Write
--         applyOne (Write1 va a) =
--           let write' = wr h va a
--            in write'

  -- wr :: h w -> V a -> a -> Write

-- It is critical that history is not modified, only write
infixl 2 <---
(<---) :: (Show a) => V a -> V a -> TMI w ()
vlvalue <--- vrvalue = do
  ss <- get
  let rvalue = r (history (execState ss)) vrvalue
      write' = Write [Write1 vlvalue rvalue]
  put $ ss { writes = (writes ss <> write') }
  -- liftIO $ runListeners history'
  return ()

data Listener = forall a. Listener (V a) (a -> IO ())

mkFielder :: String -> (r -> a) -> (r -> a -> r) -> V (R r -> R a)
mkFielder s fieldFor fieldRev = VConst s __acc
  where __acc (R r rr) = (R a ra)
          where a = fieldFor r
                ra = Receiver s $ \newA ->
                  rr <-- fieldRev r newA

{-
listen :: V a -> (a -> IO ()) -> TMI w ()
listen v action = do
  ts <- get
  let listener = Listener v action
      -- history' = addListener (history ts) listener
  put $ ts { listeners = listener:(listeners ts) }

--tmiRun :: h w -> TMI h w a -> IO (a, h w)
persistentTmiRun :: (Show w, Read w) => FilePath -> TMI w a -> IO a
persistentTmiRun filename action = do
  history <- readHistory filename
  (a, history') <- tmiRun history action
  writeHistory filename history'
  return a

readHistory :: (Read w) => FilePath -> IO (History w)
readHistory filename = do
  s <- readFile' filename
  return $ fromString s

writeHistory :: (Show w) => FilePath -> History w -> IO ()
writeHistory filename h = writeFile filename (toString h)

-}
