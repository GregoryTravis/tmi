{-# Language GADTs, NamedFieldPuns, RankNTypes #-}

module Runtime
( mainLoop
-- for recon
, advanceExtBind
, advanceRetBind
, advanceWriteBind
, advanceCallCC
, getForkTMI
, getForkNext
-- , getReadV
-- , getReadK
, doRead
, doLogK ) where

import Control.Monad (when)
import Control.Monad.State.Lazy
import Data.Typeable
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
    (todo:todos) -> liftIO $ msp $ rd (generations h) Latest todo

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
                  | Readed (V w (TMI w ()))
                  | Logged (V w (TMI w ()))
                  | Froze (V w (TMI w ()))
                  -- | forall a. Readed (V w (V w a)) (V w (a -> TMI w ()))

stepTMI :: [w] -> Generation -> V w (TMI w ()) -> StepResult w
stepTMI ws gen vcps =
  let cps = rd ws gen vcps
   in case cps of
        (Bind (Step (Ret _)) _) -> Stepped (advanceRetBindV vcps)
        (Bind (Step e@(Ext _)) k) -> Called vcps
        (Bind (Step (WriteStep write)) k) -> Wrote write (advanceWriteBindV vcps)
        (Bind (Step (CallCC kr)) k) -> CalledCC (advanceCallCCV vcps)
        (Bind (Step (Fork _)) _) -> Forked (getForkTMIV vcps) (getForkNextV vcps)
        (Bind (Step (Read _)) _) -> Readed vcps -- (getReadVV vcps) (getReadKV vcps)
        (Bind (Step (Log _)) _) -> Logged vcps
        (Bind (Step (Freeze _)) _) -> Froze vcps
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

-- getReadV :: TMI w () -> V w a
-- getReadV (Bind (Step (Read va)) _) = va

-- getReadVV :: V w (TMI w ()) -> V w (V w a)
-- getReadVV = ulift1 "getReadV" getReadV

-- getReadK :: TMI w () -> (a -> TMI w ())
-- getReadK (Bind (Step (Read _)) k) = k

-- getReadKV :: V w (TMI w ()) -> V w (a -> TMI w ())
-- getReadKV = ulift1 "getReadK" getReadK

-- TODO don't run an Ext immedaitely
runATodo :: (Eq w, Read w, Typeable w, Show w) => ExtRunner (Tag, String) -> St w ()
runATodo er = do
  h@H { calls, todo, generations } <- get
  let latestW = case generations of (w:ws) -> w
                                    [] -> error "runATodo: no generations"
  case todo of
    [] -> return ()
    (vcps:vcpss) -> do case stepTMI generations Latest vcps of
                        Stepped vcps' -> put $ h { todo = vcps' : vcpss }
                        Called vcps -> do
                          let (tag, cc') = check calls vcps
                          liftIO $ startCall er generations Latest tag vcps
                          put $ h { calls = cc', todo = vcpss }
                        CalledCC vcps' -> put $ h { todo = vcps' : vcpss }
                        Wrote write vcps' -> do
                          -- liftIO $ msp "propWrite"
                          let ws' = propWrite generations Latest write
                          put $ h { generations = ws', todo = vcps' : vcpss }
                        Forked vtmi vnext -> put $ h { todo = (vnext : vcpss) ++ [vtmi] }
                        Readed vcps -> do
                          -- let reader = (ulift1 "rd" rd) (VNice latestW)
                          --     vtmi = doReadV reader vcps
                          -- let cps = rd w vcps
                          --     a = case cps of (Bind (Step (Read va)) k) -> rd latestW va
                          --     vtmi = doReadV (k a) vcps
                          let -- cps = rd latestW vcps
                              vtmi = doReadV generations Latest vcps
                          put $ h { todo = vtmi : vcpss }
                        Logged vcps -> do let s = case (rd generations Latest vcps) of (Bind (Step (Log s)) _) -> s
                                          let vtmi = doLogKV vcps
                                          when verbose $ liftIO $ msp $ "Runtime log: " ++ s
                                          put $ h { todo = vtmi : vcpss }
                        Froze vcps -> do
                          let vtmi = doFreezeV generations vcps
                          put $ h { todo = vtmi : vcpss }
                        Nada -> put $ h { todo = vcpss }

doLogK :: TMI w () -> TMI w ()
doLogK (Bind (Step (Log s)) k) = k ()

doLogKV :: V w (TMI w ()) -> V w (TMI w ())
doLogKV = ulift1 "doLogK" doLogK

-- doRead :: (forall a. V w a -> a) -> TMI w () -> TMI w ()
-- doRead reader (Bind (Step (Read va)) k) = k (reader va)

-- doRead :: a -> TMI w () -> TMI w ()
-- doRead a (Bind (Step (Read va)) k) = k a

doRead :: [w] -> Generation -> TMI w () -> TMI w ()
doRead ws gen (Bind (Step (Read va)) k) = k (rd ws gen va)

doReadV :: [w] -> Generation -> V w (TMI w ()) -> V w (TMI w ())
-- doReadV :: V w (forall a. V w a -> a) -> V w (TMI w ()) -> V w (TMI w ())
doReadV ws gen = ulift1 "doRead" (doRead ws gen)

doFreeze :: [w] -> TMI w () -> TMI w ()
doFreeze ws (Bind (Step (Freeze va)) k) = k (VFreeze gen va)
  where gen = length ws - 1

doFreezeV :: [w] -> V w (TMI w ()) -> V w (TMI w ())
doFreezeV ws = ulift1 "doFreeze" (doFreeze ws)

-- doRead :: w -> V w (V w a) -> V w (a -> TMI w ()) -> TMI w ()
-- doRead w vva vk =
--   let va = rd w vva
--    in vk <$> va

externalize :: (TMI w ()) -> Tag -> IO (Tag, String)
externalize (Bind (Step (Ext ioa)) _) tag = do
  -- msp "RUNNING IOTS"
  a <- ioa
  let s = show a
  return (tag, s)

startCall :: ExtRunner (Tag, String) -> [w] -> Generation -> Tag -> V w (TMI w ()) -> IO ()
startCall er ws gen tag vcps = do
  let cps = rd ws gen vcps
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

loop :: (Eq w, Ord w, Typeable w, HasRecon w, Read w, Show w) => ExtRunner (Tag, String) -> St w ()
loop er = do
  doLog
  when verbose doStats
  -- testBounce
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

doStats :: St w ()
doStats = do
  h <- get
  liftIO $ msp (stats h)

mainLoop :: (Eq w, Ord w, Typeable w, HasRecon w, Read w, Show w) => H w -> IO ()
mainLoop h = do
  er <- mkExtRunner
  ((), h') <- runStateT (loop er) h
  when verbose2 $ msp $ "final state:"
  when verbose2 $ msp h'
  -- msp "loop done"
  return ()
