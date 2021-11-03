{-# LANGUAGE NumericUnderscores #-}

module Test
( testMain ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant.Divisible (divide)

import Appendo
import Lens
import Lib
import Tmi
import Util

data W = W
  { invitedUsers :: [String]
  , aList :: [Int]
  , anotherList :: [Int]
  , aThirdList :: [Int]
  , anEmptyList :: [Int]
  , zero :: Int
  , anAppendo :: Appendo Int
  }
  deriving Show

world = W
  { invitedUsers = []
  , aList = [30, 40, 50]
  , anotherList = [2, 3, 4]
  , aThirdList = [12, 13, 14, 15]
  , anEmptyList = []
  , zero = 0
  , anAppendo = Appendo [1]
  }

history :: History W
history = mkHistory world

vw :: V W
vw = getRoot history

_invitedUsers = mkFielder "_invitedUsers" invitedUsers $ \w a -> w { invitedUsers = a }
_aList = mkFielder "_aList" aList $ \w a -> w { aList = a }
_anotherList = mkFielder "_anotherList" anotherList $ \w a -> w { anotherList = a }
_aThirdList = mkFielder "_aThirdList" aThirdList $ \w a -> w { aThirdList = a }
_anEmptyList = mkFielder "_anEmptyList" anEmptyList $ \w a -> w { anEmptyList = a }
_zero = mkFielder "_zero" zero $ \w a -> w { zero = a }
_anAppendo = mkFielder "_anAppendo" anAppendo $ \w a -> w { anAppendo = a }

invitedUsersV :: V [String]
invitedUsersV = _invitedUsers <$$> vw

shiftNAdd :: R Int -> R Int -> R Int
shiftNAdd (R x rx) (R y ry) = R z rz
  where z = y * 10 + x
        rz = divide dv rx ry
          where dv z' = (x', y')
                  where x' = z' `mod` 10
                        y' = z' `div` 10
        -- Works
        -- rz = Receiver "shiftNAdd" $ \z' -> let x' = z' `mod` 10
        --                                        y' = z' `div` 10
        --                                     in (rx <-- x') <> (ry <-- y')

shiftNAddV :: V (R Int -> R Int -> R Int)
shiftNAddV = vconst "shiftNAddV" shiftNAdd

-- (<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
modded = mapV <**> modStringV <$$> invitedUsersV

testMain = do
  () <- tmiMain (return history) action
  -- eventLoop history'
  -- history'' <- tmiRunIO history' refresh
  msp "ext hi"

action :: TMI W ()
action = do
  liftIO $ msp "yooo"
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV $ slisteny "invitedUsersV"
  listen modded (slisteny "modded")
  -- listen (ifV <**> vconst True <**> vconst 2 <$$> vconst 3) listeny
  -- listen (ifV <**> vconst False <**> vconst 2 <$$> vconst 3) listeny
  -- listen (headV <$$> vconst [3, 4, 5]) listeny
  -- listen (tailV <$$> vconst [3, 4, 5]) listeny
  listen (_aList <$$> vw) listeny
  listen (_anotherList <$$> vw) $ slisteny "_anotherList"
  listen (_zero <$$> vw) listeny
  -- listen (headV <$$> (_aList <$$> vw)) listeny
  -- listen (tailV <$$> (_aList <$$> vw)) listeny
  -- listen (consV <**> (headV <$$> (_aList <$$> vw))
  --               <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) listeny
  -- let mappuh = composeV <**> incV <$$> (addV 4) -- works
  let mappuh = composoV <$$> incers
      incers = consV <**> incV <$$> (consV <**> addV 5 <$$> vconst "[]" [])
  let mapped = mapVE mappuh (_aList <$$> vw)
  let aFold :: V Int
      aFold = foldrVE shiftNAddV (_zero <$$> vw) (_anotherList <$$> vw)
  listen mapped listeny
  listen aFold listeny
  let aFoldo :: V Int
      aFoldo = foldoVE shiftNAddV (_zero <$$> vw) (_anotherList <$$> vw)
  listen aFoldo $ slisteny "aFoldo"
  -- let fooo = composeVE consV (shiftNAddV <**> vconst "" 4)
  --     mappedViaFold = foldoVE (VUnPartialApp fooo) (vconst "" []) (_aList <$$> vw)
  let mappedViaFold = mapViaFoldVE incV (_aList <$$> vw)
  listen mappedViaFold listeny
  let reverseMVF = reverseV <$$> mappedViaFold
  listen reverseMVF listeny
  let reverseVEMVF = reverseVE mappedViaFold
  listen reverseVEMVF listeny
  let reverseAccVEMVF = reverseAccVE mappedViaFold
  listen reverseAccVEMVF listeny
  let doubleReverseMVF = reverseV <$$> reverseMVF
  listen doubleReverseMVF listeny
  let doubleReverseVEMVF = reverseV <$$> reverseVEMVF
  listen doubleReverseVEMVF listeny
  listen (_aThirdList <$$> vw) listeny
  listen (_anEmptyList <$$> vw) listeny
  let appended = appendV <**> (_anotherList <$$> vw) <$$> (_aThirdList <$$> vw)
  listen appended listeny
  let appended2 = appendV <**> (_aThirdList <$$> vw) <$$> (_anEmptyList <$$> vw)
  listen appended2 listeny
  let zippie = zipWithV plusV (_aList <$$> vw) (_anotherList <$$> vw)
  listen zippie listeny
  let zippie2 = zipV (_aList <$$> vw) (_anotherList <$$> vw)
  listen zippie2 listeny
  let inxed = inxV <**> (_aList <$$> vw) <$$> vconst "" 1
  listen inxed listeny
  let firsty = fstV <$$> (inxV <**> zippie2 <$$> vconst "" 1) 
  listen firsty listeny
  let secondy = sndV <$$> (inxV <**> zippie2 <$$> vconst "" 1) 
  listen secondy listeny
  let appo = _anAppendo <$$> vw
  listen appo listeny
  appendo appo $ vconst "" 2
  -- Writes
  -- secondy <--- vconst "" 333 -- works
  -- firsty <--- vconst "" 4444 -- works
  -- inxed <--- vconst "" 400 -- works
  -- (inxV <**> zippie2 <$$> (VCheckConst "" 1)) <--- vconst "" (400, 3000) -- works
  -- zippie2 <--- vconst "" [(30,20), (40, 30), (50, 40)]
  -- zippie <--- vconst "" [51, 61, 71] -- works
  -- appended <--- vconst "" [12,3,4,12,513,14,15] -- works
  -- appended <--- vconst "" [0, 1, 2, 3, 4, 5, 6] -- works
  -- appended2 <--- vconst "" [12,513,14,15] -- works
  let consied = consV <**> vcheckconst "boop" <$$> modded
  listen consied (slisteny "consied")
  invitedUsersV <--- vconst "" ["b", "heyo", "hippo"]
  -- modded <--- vconst "" ["c!", "deyo!", "lippo!"]
  consied <--- vconst "" ["boop", "c!", "ddeyo!", "lippo!"]
  -- uhh <- get
  -- liftIO $ msp ("num listeners", (length (listeners (execState uhh))))
  -- mapped <--- vconst "" [302, 402, 502] -- works
  -- mappedViaFold <--- vconst "" [5964,6964,7964] -- works
  -- reverseMVF <--- vconst "" [7964,6964,5964]
  -- aFold <--- vconst "" (456::Int) -- Works
  aFoldo <--- vconst "" (789::Int) -- Works
  -- (headV <$$> (_aList <$$> vw)) <--- vconst 31
  -- (tailV <$$> (_aList <$$> vw)) <--- vconst [42, 52]
  -- Non-singular write
  -- (consV <**> (headV <$$> (_aList <$$> vw))
  --        <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) <--- vconst [310, 520]

_testMain = do
  -- msp $ fyold (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ fyold' (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ foldo (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ mapViaFold (+1) [4, 5, 6]
  -- msp $ reverseAcc [5, 6, 7]
  -- msp $ "result " ++ show a
  -- msp history'
  -- runListeners history'
  -- msp $ case history' of History _ listeners -> length listeners
  -- msp $ map (+1) [3, 4, 5]
  -- msp $ mapE (+1) [3, 4, 5]
  -- msp $ mapE' (+1) [3, 4, 5]
  msp "ext hi"
