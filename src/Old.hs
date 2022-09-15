{-# Language FlexibleInstances, NamedFieldPuns, StandaloneDeriving, ScopedTypeVariables,
             TypeApplications #-}

module Old
( oldMain ) where

import Control.Concurrent
import Unsafe.Coerce

import H
import Lens
import Lift
import Lib
import NiceMap
import Parr
import Propagate
import Recon
import Runtime
import Storage
import TMI
import Ty hiding (V, H, TMI)
import qualified Ty (V, H(..), TMI(..))
import Util
import V
import VNiceMap
import VReadShow

-- start boilerplate
type V = Ty.V W
type H = Ty.H W
type TMI = Ty.TMI W
vroot :: V W
vroot = VRoot
-- end boilerplate

theWorld :: W
theWorld = W { anInt = 12, anm = empty }

theHistory :: H
theHistory = initHistory vTheMain theWorld

data W = W
  { anInt :: Int
  , anm :: NiceMap
  }
  deriving (Eq, Ord, Read, Show)
fanInt app = field app "anInt" anInt $ \w anInt -> w { anInt }
vanInt = fanInt vroot
fanm app = field app "anm" anm $ \w anm -> w { anm }
vanm = fanm vroot

instance HasRecon W where
  -- TODO is it bad? Is it wrong?
  getRecon "cps" = unsafeCoerce $ VNamed "cps" (cps :: TMI Int -> TMI ())
  getRecon "nope" = unsafeCoerce $ VNamed "nope" nope
  getRecon "theMain" = unsafeCoerce $ VNamed "theMain" theMain
  getRecon "advanceExtBind" = unsafeCoerce $ VNamed "advanceExtBind" advanceExtBind
  getRecon "advanceWriteBind" = unsafeCoerce $ VNamed "advanceWriteBind" advanceWriteBind
  getRecon "advanceRetBind" = unsafeCoerce $ VNamed "advanceRetBind" advanceRetBind
  getRecon "advanceCallCC" = unsafeCoerce $ VNamed "advanceCallCC" advanceCallCC
  getRecon "incer" = unsafeCoerce $ VNamed "incer" incer
  getRecon "getForkTMI" = unsafeCoerce $ VNamed "getForkTMI" getForkTMI
  getRecon "getForkNext" = unsafeCoerce $ VNamed "getForkNext" getForkNext
  -- getRecon "getReadV" = unsafeCoerce $ VNamed "getReadV" getReadV
  -- getRecon "getReadK" = unsafeCoerce $ VNamed "getReadK" getReadK
  getRecon "doRead" = unsafeCoerce $ VNamed "doRead" doRead
  getRecon name = error $ "recon: unimplemented: " ++ name

incer = lift1 $ nuni "(+(1::Int))" (+(1::Int))

lewp :: Int -> TMI ()
lewp i = do
  call $ msp $ "lewp " ++ show i
  lewp (i + 1)

lewpTo :: Int -> Int -> TMI ()
lewpTo i end = do
  if i >= end
    then return ()
    else do
           call $ msp $ "lewp " ++ show i
           lewpTo (i + 1) end

mutRecFoo :: Int -> Int -> TMI ()
mutRecFoo i end = do
  if i >= end
    then return ()
    else do
           call $ msp $ "lewp foo " ++ show i
           mutRecBar (i + 1) end

mutRecBar :: Int -> Int -> TMI ()
mutRecBar i end = do
  if i >= end
    then return ()
    else do
           call $ msp $ "lewp bar " ++ show i
           mutRecFoo (i + 1) end

deepK0 :: Int -> TMI ()
deepK0 i = do
  a <- do b <- deepK1 i
          call $ msp $ "b " ++ show b
          Step $ Ret $ b + 1
  call $ msp $ "a " ++ show a

deepK1 :: Int -> TMI  Int
deepK1 i = do
  c <- do d <- Step $ CallCC (\k -> do if i == 20
                                         then k i
                                         else do call $ msp "stopping"
                                                 Step $ Ret ())
          call $ msp $ "d " ++ show d
          Step $ Ret $ d + 1
  call $ msp $ "c " ++ show c
  Step $ Ret i

slep :: Int -> TMI ()
slep n = call $ threadDelay (n * 1000000)

vslep :: Int -> TMI ()
vslep n = do
  call $ msp "a"
  slep n
  call $ msp "b"

sleepAndRet :: Int -> TMI Int
sleepAndRet n = do
  slep n
  Step $ Ret n

theMain :: TMI ()
theMain = do
  (a, b) <- parr vanm (sleepAndRet 2) (sleepAndRet 3)
  call $ msp $ "welp " ++ show (a, b)

  -- w <- Step $ Read VRoot
  -- call $ msp $ "heh " ++ show w
  -- i <- Step $ Read vanInt
  -- call $ msp $ "heh " ++ show i

  -- mutRecFoo 0 5
  -- Step $ Fork (vslep 3)
  -- Step $ Fork (vslep 4)

  -- works
  -- a <- do b <- Step $ Ret 1
  --         call $ msp $ "inner " ++ show b
  --         c <- do call $ msp $ "innerinner " ++ show b
  --                 call $ msp $ "innerinner2 " ++ show b
  --                 Step $ Ret b
  --         Step $ Ret c
  -- call $ msp $ "outer " ++ show a

  -- -- -- callcc
  -- -- -- calls the k
  -- a <- Step $ (CallCC (\k -> k (14::Int))) :: TMI Int
  -- call $ msp $ "welp " ++ show a

  -- -- -- doesn't call the k, does something else
  -- -- a <- Step $ CallCC (\k -> call $ msp "noper2")
  -- -- -- doesn't call the k, does something else
  -- -- a <- Step $ CallCC (\k -> do () <- call $ msp "noper"
  -- --                              return ())
  -- call $ msp $ "welp2" -- ++ show a -- doesn't know what a is

  -- -- Does Ret work? yes.
  -- a <- Step $ Ret (12::Int)
  -- call $ msp $ "twelve " ++ show a

  -- -- -- works
  -- s <- call $ readFile "asdf"
  -- vanInt <-- incer vanInt
  -- call $ msp $ "ooo " ++ s
  -- s' <- call $ readFile "asdf"
  -- call $ msp $ "oooo " ++ s'
  -- s'' <- call $ readFile "asdf"
  -- call $ msp $ "oooo " ++ s''
  -- call $ msp $ "ooo done"

  return ()

-- theParMain = do
--   (a, b) <- par (call $ readFile "asdf") (call $ readFile "zxcv")

vTheMain :: V (TMI ())
vTheMain = VNamed "theMain" theMain

oldMain :: IO ()
oldMain = do
  mainLoop theHistory
  msp "hi oldMain"
