{-# Language ExistentialQuantification, GADTs #-}

module Log
( logMain
) where

import Data.Dynamic
import Data.Maybe (fromJust)

import Util

-- todo
-- - store the dynamic arg in a string
-- - represent the (a -> b) as a looked-up function (c -> a -> b) with c Nice, and a c,
--   storing the function name and c as a string
-- - be able to to apply the latter (with withDynArg) to the former
-- - put em in a file
--
-- The first one is the non-trivial one; oh wait, we know it's Nice tho?

-- lala' :: Typeable a => (a -> b) -> (Dynamic -> b)
-- lala' = undefined

withNiceArg :: forall a b. (Show a, Read a) => (a -> b) -> (String -> b)
withNiceArg f = f . read

withDynArg :: forall a b. Typeable a => (a -> b) -> (Dynamic -> b)
withDynArg f = f'
  where f' dx = case fromDynamic dx of Just x -> f x
                                       Nothing -> error "oof"

oof = withDynArg (+(1::Int))
tlSteps = [oof]
tlVals = [toDyn (12 :: Int)]

incInt :: Int -> Int
incInt = (+1)

-- This correctly parses a nice argument and passes it to a function
-- And it works for both int and (bool, bool)
data Step = forall a b. Read a => Step (IO a, a -> IO ())

resolveStep :: String -> Step -> IO ()
resolveStep sx (Step (_, k)) = do
  let x = read sx
   in k x

moreSteps = [Step (return (False, False), msp)]
moreRets = [show (True, True)]

reconstitute :: String -> Dynamic
reconstitute "aa" = toDyn aa
reconstitute s = error $ show ("recon", s)

aFun :: Int -> Bool
aFun = (<4)
aVal :: Int
aVal = 2
aVal5 :: Int
aVal5 = 5

recon :: String -> Dynamic
recon "aFun" = toDyn aFun
recon "aVal" = toDyn aVal
recon "aVal5" = toDyn aVal5
recon _ = error "recon"

aRes = fromJust ((fromDynamic $ fromJust $ dynApply (recon "aFun") (recon "aVal"))::Maybe Bool)
aRes5 = fromJust ((fromDynamic $ fromJust $ dynApply (recon "aFun") (recon "aVal5"))::Maybe Bool)

newtype W = W { aa :: Int }

data Q a where
  QRoot :: Q W
  QNice :: (Show a, Read a) => a -> Q a
  QNamed :: String -> a -> Q a
  QApp :: Q (a -> b) -> Q a -> Q b

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QApp qf qx) = "(QApp " ++ show qf ++ " " ++ show qx ++ ")"

r :: W -> Q a -> a
r w QRoot = w
r w (QNice x) = x
r w (QNamed _ x) = x
r w (QApp qf qx) = r w qf (r w qx)

root = QRoot
vaa = QNamed "aa" aa
one = QNice (1::Int)
plus = QNamed "plus" (+(1::Int))
faa = QApp vaa root
inced = QApp plus faa
w = W { aa = 13 }

data S = SRoot | SNice String | SNamed String | SApp S S
  deriving (Read, Show)

qs :: Q a -> S
qs QRoot = SRoot
qs (QNice x) = SNice (show x)
qs (QNamed name _) = SNice name
qs (QApp qf qx) = SApp (qs qf) (qs qx)

sq :: S -> Dynamic
sq SRoot = toDyn QRoot
-- sq (SNice s) = toDyn (read s)
sq _ = error "sq"

sfaa = qs faa
qfaa = sq sfaa

logMain = do
  msp sfaa
  -- msp qfaa
  msp $ r w inced
  -- works
  -- let i = 12 :: Int
  --     step = Step (return i, msp)
  --     step' = Step (return (False, True), msp)
  --     sx' = show (13 :: Int)
  --     resultAction = resolveStep sx' step
  --     resultAction' = resolveStep (show (True, False)) step'
  -- resultAction
  -- resultAction'
  -- resolveStep (head moreRets) (head moreSteps)

  -- -- Works
  -- let i = (12::Int)
  --     di = toDyn i
  --     sum = oof di
  -- msp sum
  -- msp $ head tlSteps (head tlVals)
  -- msp $ withNiceArg incInt "140"
  msp "log hi"
