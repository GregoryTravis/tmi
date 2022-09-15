{-# Language GADTs, NamedFieldPuns #-}

module Runtime
( mainLoop
-- for recon
, advanceExtBind
, advanceRetBind
, advanceWriteBind
, advanceCallCC
, getForkTMI
, getForkNext ) where

import Control.Monad.State.Lazy
import System.IO
import System.Directory
import System.IO.Unsafe

import CoatCheck hiding (null)
import qualified CoatCheck
import ExtRunner
import H
import Lib
import Lift
import qualified NiceMap as NM
import Propagate
import Recon
import TMI
import Ty
import Util
import VReadShow

verbose = False
verbose2 = False

type St w a = StateT (H w) IO a

showHistory :: Show w => St w ()
showHistory = get >>= (liftIO . msp)

showNextTodo :: St w ()
showNextTodo = do
  h@H { todo } <- get
  case todo of
    [] -> liftIO $ msp "no todos"
    (todo:todos) -> liftIO $ msp $ rd (last (generations h)) todo

doLog :: Show w => St w ()
doLog = do
  if verbose
    then do
           showHistory
           showNextTodo
    else return ()

addTodo :: V w (TMI w ()) -> St w ()
addTodo tmi = do
  h@H { todo } <- get
  put $ h { todo = todo ++ [tmi] }

-- stepTmi :: w -> V w (TMI w ()) -> Maybe (V w (TMI w ()))
-- stepTmi w vcps =
--   let cps = rd w vcps
--    in case cps of
--         (Bind (Ret _) _) -> Just (advanceRetBindV vcps)
--         Done -> Nothing

data StepResult w = Stepped (V w (TMI w ())) | Called (V w (TMI w ())) | Wrote (Write w) (V w (TMI w ())) | Nada | CalledCC (V w (TMI w ()))
                  | Forked (V w (TMI w ())) (V w (TMI w ()))
                  deriving Show

stepTMI :: w -> V w (TMI w ()) -> StepResult w
stepTMI w vcps =
  let cps = rd w vcps
   in case cps of
        (Bind (Step (Ret _)) _) -> Stepped (advanceRetBindV vcps)
        (Bind (Step e@(Ext _)) k) -> Called vcps
        (Bind (Step (WriteStep write)) k) -> Wrote write (advanceWriteBindV vcps)
        (Bind (Step (CallCC kr)) k) -> CalledCC (advanceCallCCV vcps)
        (Bind (Step (Fork _)) _) -> Forked (getForkTMIV vcps) (getForkNextV vcps)
        (Step (Ext io)) -> error $ "???? " ++ show (unsafePerformIO io)
        Done -> Nada
        x -> error $ "?? " ++ show cps

advanceCallCC :: TMI w () -> TMI w ()
advanceCallCC (Bind (Step (CallCC kr)) k) = kr k

advanceCallCCV :: V w (TMI w ()) -> V w (TMI w ())
advanceCallCCV = ulift1 "advanceCallCC" advanceCallCC

-- Resolve a Ret immediately
advanceRetBind :: TMI w b -> TMI w b
advanceRetBind (Bind (Step (Ret x)) k) = k x

advanceRetBindV :: V w (TMI w b) -> V w (TMI w b)
advanceRetBindV = ulift1 "advanceRetBind" advanceRetBind

-- Continue after a write
advanceWriteBind :: TMI w b -> TMI w b
advanceWriteBind (Bind (Step (WriteStep _)) k) = k ()

advanceWriteBindV :: V w (TMI w b) -> V w (TMI w b)
advanceWriteBindV = ulift1 "advanceWriteBind" advanceWriteBind

-- Resolve a call with the retval string
advanceExtBind :: TMI w b -> String -> TMI w b
advanceExtBind (Bind (Step (Ext _)) k) retvalS = k (read retvalS)

advanceExtBindV :: V w (TMI w b) -> V w String -> V w (TMI w b)
advanceExtBindV = ulift2 "advanceExtBind" advanceExtBind

getForkTMI :: TMI w () -> TMI w ()
getForkTMI (Bind (Step (Fork tmi)) k) = tmi

getForkTMIV :: V w (TMI w ()) -> V w (TMI w ())
getForkTMIV = ulift1 "getForkTMI" getForkTMI

getForkNext :: TMI w () -> TMI w ()
getForkNext (Bind (Step (Fork tmi)) k) = k ()

getForkNextV :: V w (TMI w ()) -> V w (TMI w ())
getForkNextV = ulift1 "getForkNext" getForkNext

-- TODO don't run an Ext immedaitely
runATodo :: Show w => ExtRunner (Tag, String) -> St w ()
runATodo er = do
  h@H { calls, todo, generations } <- get
  let latestW = case generations of (w:ws) -> w
                                    [] -> error "runATodo: no generations"
  case todo of
    [] -> return ()
    (vcps:vcpss) -> do case stepTMI latestW vcps of
                        Stepped vcps' -> put $ h { todo = vcps' : vcpss }
                        Called vcps -> do
                          let (tag, cc') = check calls vcps
                          liftIO $ startCall er latestW tag vcps
                          put $ h { calls = cc', todo = vcpss }
                        CalledCC vcps' -> put $ h { todo = vcps' : vcpss }
                        Wrote write vcps' -> do
                          -- liftIO $ msp "propWrite"
                          let w' = propWrite latestW write
                          put $ h { generations = w' : generations, todo = vcps' : vcpss }
                        Forked vtmi vnext -> put $ h { todo = (vnext : vcpss) ++ [vtmi] }
                        Nada -> put $ h { todo = vcpss }

externalize :: (TMI w ()) -> Tag -> IO (Tag, String)
externalize (Bind (Step (Ext ioa)) _) tag = do
  -- msp "RUNNING IOTS"
  a <- ioa
  let s = show a
  return (tag, s)

startCall :: ExtRunner (Tag, String) -> w -> Tag -> V w (TMI w ()) -> IO ()
startCall er w tag vcps = do
  let cps = rd w vcps
      iots = externalize cps tag
  run er iots

waitForRetval :: ExtRunner (Tag, String) -> St w ()
waitForRetval er = do
  h@H { calls, todo } <- get
  (tag, retvalS) <- liftIO $ nextResult er
  when verbose2 $ liftIO $ msp $ "Retrieve " ++ retvalS ++ " for " ++ show tag
  let Just (vcps, calls') = retrieve calls tag
      vcps' = advanceExtBindV vcps (VNice retvalS)
      h' = h { calls = calls', todo = vcps':todo }
  put h'

testBounce :: (HasRecon w, Read w, Show w) => St w ()
testBounce = do
  h <- get
  let s = show h
  s' <- liftIO $ bounce s
  let h' = read s'
  put h'
  where bounce s = do
          writeFile tmpfile s
          s' <- readFile tmpfile
          removeFile tmpfile
          return s'
        tmpfile = "/tmp/tmibounce"

loop :: (HasRecon w, Read w, Show w) => ExtRunner (Tag, String) -> St w ()
loop er = do
  doLog
  testBounce
  atd <- anythingToDo
  if not atd
    then do
      aosc <- anyOutStandingCalls
      if aosc
        then do
          when verbose2 $ liftIO $ msp "wait for retval"
          waitForRetval er
          loop er
        else do
          when verbose2 $ liftIO $ msp "Nothing to do and nothing doing"
          return ()
    else do
      -- Handle new retvals
      -- Start new calls
      -- Run a todo TMI
      when verbose2 $ liftIO $ msp "there's a todo"
      runATodo er
      -- doLog
      loop er

anythingToDo :: St w Bool
anythingToDo = do
  H { todo } <- get
  return $ not $ null todo

anyOutStandingCalls :: St w Bool
anyOutStandingCalls = do
  H { calls } <- get
  return $ not $ CoatCheck.null calls

mainLoop :: (HasRecon w, Read w, Show w) => H w -> IO ()
mainLoop h = do
  er <- mkExtRunner
  ((), h') <- runStateT (loop er) h
  when verbose2 $ msp $ "final state:"
  when verbose2 $ msp h'
  -- msp "loop done"
  return ()
