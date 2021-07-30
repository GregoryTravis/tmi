module Test
( testMain ) where

import Control.Monad.IO.Class (liftIO)

import Appendo
import Lens
import Lib
import Tmi
import Util

data DB = DB
  { invitedUsers :: [String]
  , aList :: [Int]
  , anotherList :: [Int]
  , aThirdList :: [Int]
  , anEmptyList :: [Int]
  , zero :: Int
  , anAppendo :: Appendo Int
  }
  deriving Show

type WW = W DB

world = W
  { db = DB
      { invitedUsers = []
      , aList = [30, 40, 50]
      , anotherList = [2, 3, 4]
      , aThirdList = [12, 13, 14, 15]
      , anEmptyList = []
      , zero = 0
      , anAppendo = Appendo [1]
      }
  , rpc = initRpc
  }

history :: History WW
history = mkHistory world

vw :: V WW
vw = getRoot history

_invitedUsers = mkFielder "_invitedUsers" invitedUsers $ \w a -> w { invitedUsers = a }
_aList = mkFielder "_aList" aList $ \w a -> w { aList = a }
_anotherList = mkFielder "_anotherList" anotherList $ \w a -> w { anotherList = a }
_aThirdList = mkFielder "_aThirdList" aThirdList $ \w a -> w { aThirdList = a }
_anEmptyList = mkFielder "_anEmptyList" anEmptyList $ \w a -> w { anEmptyList = a }
_zero = mkFielder "_zero" zero $ \w a -> w { zero = a }
_anAppendo = mkFielder "_anAppendo" anAppendo $ \w a -> w { anAppendo = a }

invitedUsersV :: V [String]
invitedUsersV = _invitedUsers <$$> (_db <$$> vw)

shiftNAdd :: R Int -> R Int -> R Int
shiftNAdd (R x rx) (R y ry) = R z rz
  where z = y * 10 + x
        rz = Receiver "shiftNAdd" $ \z' -> let x' = z' `mod` 10
                                               y' = z' `div` 10
                                            in (rx <-- x') <> (ry <-- y')
shiftNAddV :: V (R Int -> R Int -> R Int)
shiftNAddV = VConst "shiftNAddV" shiftNAdd

-- (<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
modded = mapV <**> modStringV <$$> invitedUsersV

extAction :: TMI WW ()
extAction = do
  let db = _db <$$> vw
  listen (_aList <$$> db) listeny
  let rpc = (_rpc <$$> vw :: V Rpc)
      calls = _calls <$$> rpc
  listen calls listeny
  listen (lengthV <$$> calls) listeny
  call <- initCall (Req 1 "hey")
  call2 <- initCall (Req 2 "hey")
  call3 <- initCall (Req 3 "hey")
  calls <--- appendV <**> calls <$$> VConst "" [call, call2, call3]
  -- return ()

testMain = do
  -- () <- tmiMain (return history) action
  () <- tmiMain (return history) extAction
  -- eventLoop history'
  -- history'' <- tmiRunIO history' refresh
  msp "ext hi"

action :: TMI WW ()
action = do
  let db = _db <$$> vw
  liftIO $ msp "yooo"
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV listeny
  listen modded listeny
  -- listen (ifV <**> VConst True <**> VConst 2 <$$> VConst 3) listeny
  -- listen (ifV <**> VConst False <**> VConst 2 <$$> VConst 3) listeny
  -- listen (headV <$$> VConst [3, 4, 5]) listeny
  -- listen (tailV <$$> VConst [3, 4, 5]) listeny
  listen (_aList <$$> db) listeny
  listen (_anotherList <$$> db) listeny
  listen (_zero <$$> db) listeny
  -- listen (headV <$$> (_aList <$$> db)) listeny
  -- listen (tailV <$$> (_aList <$$> db)) listeny
  -- listen (consV <**> (headV <$$> (_aList <$$> db))
  --               <$$> (tailV <$$> (tailV <$$> (_aList <$$>) db))) listeny
  -- let mappuh = composeV <**> incV <$$> (addV 4) -- works
  let mappuh = composoV <$$> incers
      incers = consV <**> incV <$$> (consV <**> addV 5 <$$> VConst "[]" [])
  let mapped = mapVE mappuh (_aList <$$> db)
  let aFold :: V Int
      aFold = foldrVE shiftNAddV (_zero <$$> db) (_anotherList <$$> db)
  listen mapped listeny
  listen aFold listeny
  let aFoldo :: V Int
      aFoldo = foldoVE shiftNAddV (_zero <$$> db) (_anotherList <$$> db)
  listen aFoldo listeny
  -- let fooo = composeVE consV (shiftNAddV <**> VConst "" 4)
  --     mappedViaFold = foldoVE (VUnPartialApp fooo) (VConst "" []) (_aList <$$> db)
  let mappedViaFold = mapViaFoldVE incV (_aList <$$> db)
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
  listen (_aThirdList <$$> db) listeny
  listen (_anEmptyList <$$> db) listeny
  let appended = appendV <**> (_anotherList <$$> db) <$$> (_aThirdList <$$> db)
  listen appended listeny
  let appended2 = appendV <**> (_aThirdList <$$> db) <$$> (_anEmptyList <$$> db)
  listen appended2 listeny
  let zippie = zipWithV plusV (_aList <$$> db) (_anotherList <$$> db)
  listen zippie listeny
  let zippie2 = zipV (_aList <$$> db) (_anotherList <$$> db)
  listen zippie2 listeny
  let inxed = inxV <**> (_aList <$$> db) <$$> VConst "" 1
  listen inxed listeny
  let firsty = fstV <$$> (inxV <**> zippie2 <$$> VConst "" 1) 
  listen firsty listeny
  let secondy = sndV <$$> (inxV <**> zippie2 <$$> VConst "" 1) 
  listen secondy listeny
  let appo = _anAppendo <$$> db
  listen appo listeny
  appendo appo $ VConst "" 2
  -- Writes
  -- secondy <--- VConst "" 333 -- works
  -- firsty <--- VConst "" 4444 -- works
  -- inxed <--- VConst "" 400 -- works
  -- (inxV <**> zippie2 <$$> (VCheckConst "" 1)) <--- VConst "" (400, 3000) -- works
  -- zippie2 <--- VConst "" [(30,20), (40, 30), (50, 40)]
  -- zippie <--- VConst "" [51, 61, 71] -- works
  -- appended <--- VConst "" [12,3,4,12,513,14,15] -- works
  -- appended <--- VConst "" [0, 1, 2, 3, 4, 5, 6] -- works
  -- appended2 <--- VConst "" [12,513,14,15] -- works
  invitedUsersV <--- VConst "" ["b", "heyo", "hippo"]
  modded <--- VConst "" ["c!", "deyo!", "lippo!"]
  -- uhh <- get
  -- liftIO $ msp ("num listeners", (length (listeners (execState uhh))))
  -- mapped <--- VConst "" [302, 402, 502] -- works
  -- mappedViaFold <--- VConst "" [5964,6964,7964] -- works
  -- reverseMVF <--- VConst "" [7964,6964,5964]
  -- aFold <--- VConst "" (456::Int) -- Works
  -- aFoldo <--- VConst "" (789::Int) -- Works
  -- (headV <$$> (_aList <$$> db)) <--- VConst 31
  -- (tailV <$$> (_aList <$$> db)) <--- VConst [42, 52]
  -- Non-singular write
  -- (consV <**> (headV <$$> (_aList <$$> db))
  --        <$$> (tailV <$$> (tailV <$$> (_aList <$$>) db))) <--- VConst [310, 520]

_extMain = do
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
