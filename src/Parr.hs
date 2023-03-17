{-# LANGUAGE BlockArguments #-}

module Parr
( parr
, parrList ) where

import Control.Monad (when)
import System.IO.Unsafe

import Lib
import Lift
import NiceMap
import TMI
import Ty hiding (store)
import V
import VNiceMap
import Util

verbose = False

parr :: (Show w, Show a, Show b, Read a, Read b) => V w NiceMap -> TMI w a -> TMI w b -> TMI w (a, b)
parr vnm tmia tmib = do
  -- TODO need annotation?
  vslot <- (mkSlot vnm (Nothing, Nothing)) -- :: TMI w (V w (Maybe a, Maybe b)))
  -- slot <- Step $ Read vslot
  -- let vslot' = VNice slot
  Step $ CallCC (cr vslot)
  where cr vslot pairK = do Step $ Fork $ startHalf vslot tmia vfst pairK
                            Step $ Fork $ startHalf vslot tmib vsnd pairK

parrList :: (Show w, Show a, Read a) => V w NiceMap -> [TMI w a] -> TMI w [a]
parrList vnm (tmi:tmis) = do
  (a, as) <- parr vnm tmi (parrList vnm tmis)
  return $ a : as
-- TODO shouldn't waste a parr on the last element + nil
parrList vnm [] = Step $ Ret []

-- c might be a or b but it doesn't matter here (?)
-- TODO move forks into here?
startHalf :: Show w => V w (Maybe a, Maybe b)
          -> TMI w c
          -> (V w (Maybe a, Maybe b) -> V w (Maybe c))
          -> ((a, b) -> TMI w ())
          -> TMI w ()
startHalf vpair tmi picker pairK = do
  when verbose $ call $ msp "startHalf"
  c <- tmi
  let intoSlot = picker vpair
  -- TODO should confirm there isn't a value there already, but how?
  -- call $ msp $ "WRITE " ++ tag
  w <- Step $ Read VRoot
  -- let debug = leesp "abner" ("writing", intoSlot, w)
  let debug a = unsafePerformIO $ do
                  -- msp "writing"
                  -- msp intoSlot
                  -- msp w
                  return a
  debug $ intoSlot <--* Just c
  runIfDoneV vpair pairK

runIfDoneV :: V w (Maybe a, Maybe b)
           -> ((a, b) -> TMI w ())
           -> TMI w ()
runIfDoneV vpair pairK = do
  pair <- Step $ Read vpair
  runIfDone pair pairK

runIfDone :: (Maybe a, Maybe b)
          -> ((a, b) -> TMI w ())
          -> TMI w ()
runIfDone (Just a, Just b) pairK = do
  when verbose $ call $ msp $ "runIfDone: have both"
  pairK (a, b)
runIfDone (Just _, _) _ = do
  when verbose $ call $ msp $ "runIfDone: have left"
  return ()
runIfDone (_, Just _) _ = do
  call $ msp $ "runIfDone: have right"
  return ()
runIfDone _ _ = error "runIfDone: empty??"
