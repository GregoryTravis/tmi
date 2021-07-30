{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}

module State where

import Control.Monad.State.Lazy hiding (execState)

import ExecId
import Ext
import History
import Propagate
import Trace
import UniqueId
import V
import W

listeny :: Show a => a -> IO ()
listeny x = putStrLn $ "Listeny: " ++ show x

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

updateRpc :: ExecState (W ww) -> IO (ExecState (W ww))
updateRpc es@ExecState {..} = do
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

data Listener = forall a. Listener (V a) (a -> IO ())

initCall :: Req -> TMI w Call
initCall req = do
  uid <- uniqueId
  return $ Call uid req Nothing Nothing False
