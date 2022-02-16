{-# Language GADTs, NamedFieldPuns, RankNTypes #-}

module Ledger (ledgerMain) where

import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)
import Unsafe.Coerce

import Alloc
import Core
import Lens
import Lift
import MainLoop
import Monad
import Parr
import Propagate
import Storage
import Ty hiding (V, Bi, R)
import qualified Ty as Ty
import Util
import V
import Veq

type V = Ty.V W
type Bi = Ty.Bi W
type R = Ty.R W
root :: V W
root = VRoot

data W = W
  { --requests :: [Bind]
  --, responses :: [(Int, String)]
  }
  deriving (Read, Show)

theWorld :: W
theWorld = W
  { --requests = []
  --, responses = []
  }

-- brequests = field root "requests" requests $ \w requests -> w { requests }
-- bresponses = field root "responses" responses $ \w responses -> w { responses }

-- TODO we're not actually using the rets
data Ledger = Ledger [V (TMI ())] [(Int, String)]

ledgerToString :: Ledger -> String
ledgerToString (Ledger steps rets) = show (map qToString steps, rets)
stringToLedger :: Reconstitutor -> String -> Ledger
stringToLedger recon s =
  let (stepStrings, rets) = read s
      steps = map (stringToQ recon) stepStrings
   in Ledger steps rets

pushStep :: Ledger -> V (TMI ()) -> Ledger
pushStep (Ledger steps rets) step = Ledger (steps ++ [step]) rets

pushRet :: Ledger -> (Int, String) -> Ledger
pushRet (Ledger steps rets) ret = Ledger steps (rets ++ [ret])

-- -- TODO instead of taking an index, pick the first one that doesn't have a retval; etc
-- runStep :: w -> Ledger -> Int -> IO Ledger
-- runStep w (Ledger steps rets) stepIndex = do
--   -- TODO missing cases
--   let vstep = steps !! stepIndex
--       vaction = bindAction vstep
--       vk = bindK vstep
--       Ext io = rd w vaction
--   a <- io
--   let vNextTMI = vk (k a)
--   return $ Ledger (steps ++ [vNextTMI]) (rets)
--   -- TODO don't use rd
--   -- let vstep = steps !! stepIndex
--   --     step = rd w vstep
--   --     Bind (Ext io) k = step
--   -- a <- io
--   -- let next = k a

runStep :: W -> Ledger -> Int -> IO Ledger
runStep w (Ledger steps rets) stepIndex = do
  let vstep = steps !! stepIndex
  aS <- startBind $ rd w vstep
  let vNextTMI = invokeBindV vstep (k aS)
  return $ Ledger (steps ++ [vNextTMI]) rets

data TMI a where
  Bind :: TMI b -> (b -> TMI a) -> TMI a
  Ext :: (Show a, Read a) => IO a -> TMI a
  Ret :: a -> TMI a

-- bindK = fv1 "bindK" (\(Bind _ k) -> k)
-- bindAction = fv1 "bindAction" (\(Bind a _) -> a)

-- Supply a retval to move to the next action
invokeBind :: TMI b -> String -> TMI b
invokeBind (Bind (Ext _) k) retvalS = k (read retvalS)
invokeBindV = fv2 "invokeBind" invokeBind

-- Runs the (TMI a) ext (not the b one!) and returns the result as a string
startBind :: TMI b -> IO String
startBind (Bind (Ext io) _) = do
  a <- io
  return $ show a

instance Functor TMI where
  fmap = liftM

instance Applicative TMI where
  pure  = return
  (<*>) = ap

bernd :: TMI b -> (b -> TMI a) -> TMI a
bernd (Ext io) a2tb = Bind (Ext io) a2tb

instance Monad TMI where
  (>>=) = bernd
  return = Ret

aTMI :: Int -> TMI ()
aTMI n = do
  n' <- Ext (return $ n + 1)
  () <- Ext (msp ("hey", n))
  return ()

fv1 :: String -> (a -> b) -> (V a -> V b)
fv1 s f = lift1 $ bi (VNamed s f) nope
fv2 :: String -> (a -> b -> c) -> (V a -> V b -> V c)
fv2 s f = lift2 $ bi (VNamed s f) nope
-- aTMIV = lift1 $ bi (VNamed "aTMI" aTMI) nope
aTMIV = fv1 "aTMI" aTMI

reconstitutor :: String -> a
reconstitutor "aTMI" = unsafeCoerce $ VNamed "aTMI" aTMI
reconstitutor "aTMI_" = unsafeCoerce $ VNamed "aTMI_" (error "no reverse for aTMI")
reconstitutor "invokeBind" = unsafeCoerce $ VNamed "invokeBind" invokeBind
reconstitutor "invokeBind_" = unsafeCoerce $ VNamed "invokeBind_" (error "no reverse for invokeBind")
reconstitutor "nope" = unsafeCoerce $ VNamed "nope" (error "no reverse for nope")
reconstitutor s = error $ "??" ++ s

ledgerMain = do
  let ledger = Ledger [] []
      ledger2 = pushStep ledger (aTMIV (k 12))
      ledger2s = ledgerToString ledger2
      ledger2' = stringToLedger reconstitutor ledger2s
  ledger3 <- runStep theWorld ledger2 0
  ledger3' <- runStep theWorld ledger2' 0
  let ledger3'' = stringToLedger reconstitutor (ledgerToString ledger3')
  ledger4 <- runStep theWorld ledger3' 1
  -- ledger5 <- runStep theWorld ledger4 2
  msp ledger2s
  -- msp $ ledgerToString ledger3
  -- msp $ ledgerToString ledger3'
  msp $ ledgerToString ledger3''
  msp $ ledgerToString ledger4
  -- msp $ ledgerToString ledger5
  msp "hi ledger"
