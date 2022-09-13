module Parr
() where

import Lib
import Lift
import NiceMap
import TMI
import Ty hiding (store)
import V
import Util

-- parr :: V w NiceMap -> TMI w a -> TMI w b -> TMI w (a, b)
-- parr nm tmia tmib = do
--   -- TODO need annotation?
--   vslot <- (mkSlot vanm :: TMI (V (Maybe a, Maybe b)))
--   CallCC cr
--   where cr pairK = do Fork $ startHalf vslot tmia vfst pairK
--                       Fork $ startHalf vslot tmib vsnd pairK

-- -- c might be a or b but it doesn't matter here (?)
-- startHalf :: V w (Maybe a, Maybe b)
--           -> TMI w c
--           -> (V w (a, b) -> V w c)
--           -> V w ((a, b) -> TMI w ())
--           -> TMI w ()
-- startHalf vpair tmi picker pairK = do
--   liftIO $ msp "startHalf"
--   c <- tmi
--   let intoSlot = picker vpair
--   -- TODO should confirm there isn't a value there already, but how?
--   intoSlot <-- Just c
--   doneTmi <- runIfDoneV vpair pairK
--   doneTmi

-- runIfDoneV :: V w (Maybe a, Maybe b)
--            -> V w ((a, b) -> TMI w ())
--            -> V (TMI w ())
-- runIfDoneV = lift2 $ nuni "runIfDone" runIfDone

-- runIfDone :: (Maybe a, Maybe b)
--           -> ((a, b) -> TMI w ())
--           -> TMI w ()
-- runIfDone (Just a, Just b) pairK = do
--   liftIO $ msp $ "runIfDone: have both"
--   pairK (a, b)
-- runIfDone (Just _, _) = do
--   liftIO $ msp $ "runIfDone: have left"
--   return ()
-- runIfDone (_, Just _) = do
--   liftIO $ msp $ "runIfDone: have right"
--   return ()
-- runIfDone _ = error "runIfDone: empty??"

--   -- vsl <- (mkSlot vanm :: TMI (V Int))
--   -- () <- Step $ WriteStep (Write vsl 23)
