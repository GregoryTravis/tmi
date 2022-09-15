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

parr :: (Show a, Show b, Read a, Read b) => String -> V w NiceMap -> TMI w a -> TMI w b -> TMI w (a, b)
parr tag vnm tmia tmib = do
  -- TODO need annotation?
  vslot <- (mkSlot vnm (Nothing, Nothing)) -- :: TMI w (V w (Maybe a, Maybe b)))
  -- slot <- Step $ Read vslot
  -- let vslot' = VNice slot
  Step $ CallCC (cr vslot)
  where cr vslot pairK = do Step $ Fork $ startHalf vslot tmia vfst pairK (tag ++ "0")
                            Step $ Fork $ startHalf vslot tmib vsnd pairK (tag ++ "1")

-- c might be a or b but it doesn't matter here (?)
-- TODO move forks into here?
startHalf :: V w (Maybe a, Maybe b)
          -> TMI w c
          -> (V w (Maybe a, Maybe b) -> V w (Maybe c))
          -> ((a, b) -> TMI w ())
          -> String
          -> TMI w ()
startHalf vpair tmi picker pairK tag = do
  call $ msp "startHalf"
  c <- tmi
  let intoSlot = picker vpair
  -- TODO should confirm there isn't a value there already, but how?
      call $ msp $ "WRITE " ++ tag
  intoSlot <--* Just c
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
  call $ msp $ "runIfDone: have both"
  eesp "golly" $ pairK (a, b)
runIfDone (Just _, _) _ = do
  call $ msp $ "runIfDone: have left"
  return ()
runIfDone (_, Just _) _ = do
  call $ msp $ "runIfDone: have right"
  return ()
runIfDone _ _ = error "runIfDone: empty??"
