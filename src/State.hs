{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}

module State where

import Control.Monad.State.Lazy hiding (execState)

import ExecId
import History
import Propagate
import UniqueId
import Util
import V

-- TODO: call this Mainloop or something?

listeny :: Show a => a -> IO ()
listeny x = putStrLn $ "Listeny: " ++ show x

slisteny :: Show a => String -> a -> IO ()
slisteny s x = putStrLn $ s ++ ": " ++ show x

runListeners :: ExecState w -> IO ()
runListeners es = mapM_ runListener (listeners es)
  where runListener (Listener va action) = do
          let a = r (history es) va
          action a

-- State carried through an entire TMI program.
data ExecState w = ExecState
  { execId :: ExecId
  , listeners :: [Listener]
  , history :: History w
  }

-- Runs the action, commits the change, and then listens to the event stream?
tmiMain :: Show w => IO (History w) -> TMI w () -> IO ()
tmiMain hio action = do
  h <- hio
  -- ch <- (newChan :: Chan TMI w ())
  eid <- currentExecId
  let es = ExecState { execId = eid, listeners = [], history = h }
  ((), es') <- runStateT (runTMI action) es
  runListeners es'
  return ()

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

data StepState w = StepState
  { execState :: ExecState w
  , writes :: Write
  , serial :: Int
  }

type TMIE w a = StateT (ExecState w) IO a
type TMI w a = StateT (StepState w) IO a
-- runStateT :: s -> m (a, s)	

listen :: V a -> (a -> IO ()) -> TMI w ()
listen va ioAction = do
  stepState <- get
  let ls = listeners (execState stepState)
  let listener = Listener va ioAction
      ls' = ls ++ [listener]
      -- TODO we don't need this execState write, right?
      stepState' = stepState { execState = (execState stepState) { listeners = ls' } }
  put stepState'

-- TODO move to ID.hs
uniqueId :: TMI w UniqueId
uniqueId = do
  ss <- get
  genId <- getGenId
  let nextSerial = serial ss
      ss' = ss { serial = nextSerial + 1 }
  put ss'
  return $ UniqueId (genId, nextSerial)

getGenId :: TMI w Int
getGenId = do histlen . history . execState <$> get

getExecId :: TMI w ExecId
getExecId = do execId . execState <$> get

-- It is critical that history is not modified, only write
infixl 2 <---
(<---) :: (Show a) => V a -> V a -> TMI w ()
vlvalue <--- vrvalue = do
  ss <- get
  let rvalue = r (history (execState ss)) vrvalue
      write' = Write [Write1 vlvalue rvalue]
  put $ ss { writes = writes ss <> write' }
  -- liftIO $ runListeners history'
  return ()

-- TODO: Move to Lister.hs
data Listener = forall a. Listener (V a) (a -> IO ())
