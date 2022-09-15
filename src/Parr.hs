module Parr
( parr ) where

import Lib
import Lift
import NiceMap
import TMI
import Ty hiding (store)
import V
import VNiceMap
import Util

parr :: (Show a, Show b, Read a, Read b) => V w NiceMap -> TMI w a -> TMI w b -> TMI w (a, b)
parr vnm tmia tmib = do
  -- TODO need annotation?
  vslot <- (mkSlot vnm (Nothing, Nothing)) -- :: TMI w (V w (Maybe a, Maybe b)))
  Step $ CallCC (cr vslot)
  where cr vslot pairK = do Step $ Fork $ startHalf vslot tmia vfst pairK
                            Step $ Fork $ startHalf vslot tmib vsnd pairK

-- c might be a or b but it doesn't matter here (?)
-- TODO move forks into here?
startHalf :: V w (Maybe a, Maybe b)
          -> TMI w c
          -> (V w (Maybe a, Maybe b) -> V w (Maybe c))
          -> ((a, b) -> TMI w ())
          -> TMI w ()
startHalf vpair tmi picker pairK = do
  call $ msp "startHalf"
  c <- tmi
  let intoSlot = picker vpair
  -- TODO should confirm there isn't a value there already, but how?
  intoSlot <--* Just c
  runIfDoneV vpair pairK

--   let doneTmiV = runIfDoneV vpair pairK
--   doneTmi <- Step $ Read doneTmiV
--   doneTmi

-- runIfDoneV :: V w (Maybe a, Maybe b)
--            -> V w ((a, b) -> TMI w ())
--            -> V w (TMI w ())
-- runIfDoneV = lift2 $ nuni "runIfDone" runIfDone

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
  call $ msp $ "runIfDone: have both"
  pairK (a, b)
runIfDone (Just _, _) _ = do
  call $ msp $ "runIfDone: have left"
  return ()
runIfDone (_, Just _) _ = do
  call $ msp $ "runIfDone: have right"
  return ()
runIfDone _ _ = error "runIfDone: empty??"

  -- vsl <- (mkSlot vanm :: TMI (V Int))
  -- () <- Step $ WriteStep (Write vsl 23)
