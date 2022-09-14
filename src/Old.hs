{-# Language FlexibleInstances, NamedFieldPuns, StandaloneDeriving, ScopedTypeVariables,
             TypeApplications #-}

module Old
( oldMain ) where

import Unsafe.Coerce

import H
import Lens
import Lift
import Lib
import NiceMap
import Propagate
import Recon
import Runtime
import Storage
import TMI
import Ty hiding (V, H, TMI, CPS)
import qualified Ty (V, H(..), TMI(..), CPS(..))
import Util
import V
import VNiceMap
import VReadShow

-- start boilerplate
type V = Ty.V W
type H = Ty.H W
type TMI = Ty.TMI W
type CPS = Ty.CPS W
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
  -- getRecon "cps" = unsafeCoerce $ VNamed "cps" (vcps :: V (TMI Int) -> V (CPS Int))
  -- TODO is it bad? Is it wrong?
  getRecon "cps" = unsafeCoerce $ VNamed "cps" (cps :: TMI Int -> CPS ())
  getRecon "nope" = unsafeCoerce $ VNamed "nope" nope
  getRecon "theMain" = unsafeCoerce $ VNamed "theMain" theMain
  getRecon "advanceExtBind" = unsafeCoerce $ VNamed "advanceExtBind" advanceExtBind
  getRecon "advanceWriteBind" = unsafeCoerce $ VNamed "advanceWriteBind" advanceWriteBind
  getRecon "advanceRetBind" = unsafeCoerce $ VNamed "advanceRetBind" advanceRetBind
  getRecon "incer" = unsafeCoerce $ VNamed "incer" incer
  getRecon name = error $ "unimplemented " ++ name

incer = lift1 $ nuni "(+(1::Int))" (+(1::Int))

theMain :: TMI ()
theMain = do
  a <- do b <- Step $ Ret 1
          call $ msp $ "inner " ++ show b
          c <- do call $ msp $ "innerinner " ++ show b
                  call $ msp $ "innerinner2 " ++ show b
                  Step $ Ret b
          Step $ Ret c
  call $ msp $ "outer " ++ show a

  -- nested callcc stuff?
  -- a <- do b <- CallCC (\x -> ...)
  --         doStuff b :: TMI a
  -- doStuff a

  -- -- callcc
  -- -- calls the k
  -- a <- (CallCC (\k -> k (14::Int))) :: TMI Int
  -- call $ msp $ "welp " ++ show a
  -- -- doesn't call the k, does something else
  -- a <- CallCC (\k -> call $ msp "nope")
  -- call $ msp $ "welp " ++ show a

  -- Does Ret work? yes.
  a <- Step $ Ret (12::Int)
  call $ msp $ "twelve " ++ show a

  -- -- works
  s <- call $ readFile "asdf"
  -- () <- Step $ WriteStep (Write vanInt 120)
  -- vanInt <--* 120
  vanInt <-- incer vanInt
  call $ msp $ "ooo " ++ s
  s' <- call $ readFile "asdf"
  call $ msp $ "oooo " ++ s'
  s'' <- call $ readFile "asdf"
  call $ msp $ "oooo " ++ s''
  call $ msp $ "ooo done"

  -- -- TODO does not work because cps' is not complete and cannot be completed because the inner type is not Read
  -- vsl <- (mkSlot vanm :: TMI (V Int))
  -- () <- Step $ WriteStep (Write vsl 23)
  -- -- mkSlot :: forall a w. (Read a, Show a) => V w NiceMap -> TMI w (V w a)

  return ()

-- theParMain = do
--   (a, b) <- par (call $ readFile "asdf") (call $ readFile "zxcv")

vTheMain :: V (TMI ())
vTheMain = VNamed "theMain" theMain

oldMain :: IO ()
oldMain = do
  mainLoop theHistory
  msp "hi oldMain"
