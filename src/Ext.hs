{-# LANGUAGE ExistentialQuantification, NumericUnderscores #-}

module Ext
( extMain
) where

-- import Data.Dynamic
import Data.Time.Clock (UTCTime, getCurrentTime)
-- import Data.Typeable
import Control.Monad.State hiding (lift)

import Tmi
import Util

data Request a = Request (IO a)

newtype Appendo a = Appendo [a] deriving Show
appendo_1 :: V (R (Appendo a) -> R [a])
appendo_1 = VConst "appendo_1" __app
  where __app (R ap rap) = (R as ras)
          where as = case ap of Appendo as -> as
                ras = Receiver "appendo_1" $ \newAs ->
                  rap <-- Appendo newAs

-- mkFielder :: String -> (r -> a) -> (r -> a -> r) -> V (R r -> R a)
-- mkFielder s fieldFor fieldRev = VConst s __acc
  -- where __acc (R r rr) = (R a ra)
  --         where a = fieldFor r
  --               ra = Receiver s $ \newA ->
  --                 rr <-- fieldRev r newA

appendo :: (Eq a, Show a) => V (Appendo a) -> V a -> TMI W ()
appendo vapp va =
  let vas = appendo_1 <$$> vapp
   in vas <--- appendV <**> vas <$$> (consV <**> va <$$> (VCheckConst "appendo" []))

data W = W
  { invitedUsers :: [String]
  , aList :: [Int]
  , anotherList :: [Int]
  , aThirdList :: [Int]
  , anEmptyList :: [Int]
  , zero :: Int
  , anAppendo :: Appendo Int
  , rpc :: Rpc
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
  , rpc = initRpc
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
_rpc = mkFielder "_rpc" rpc $ \w a -> w { rpc = a }

mkFielder :: String -> (r -> a) -> (r -> a -> r) -> V (R r -> R a)
mkFielder s fieldFor fieldRev = VConst s __acc
  where __acc (R r rr) = (R a ra)
          where a = fieldFor r
                ra = Receiver s $ \newA ->
                  rr <-- fieldRev r newA

no_rev :: String -> a
no_rev s = error $ "No reverse for " ++ s

invitedUsersV :: V [String]
invitedUsersV = _invitedUsers <$$> vw

listeny :: Show a => a -> IO ()
listeny x = putStrLn $ "Listeny: " ++ (show x)

modString_for :: String -> String
modString_for = (++ "!")
modString_rev :: R String -> String -> Write
modString_rev (R _ rs) newS =
  rs <-- init newS
modStringV :: V (R String -> R String)
modStringV = VConst "modStringV" $ hybrid1 modString_for modString_rev

-- requests = map toRequest invitedUsersV
-- duped = mapV modStringV invitedUsersV
-- mapV :: V (R String -> R String) -> V [String] -> V [String]
-- mapV :: V (R a -> R a) -> V [a] -> V [a]
-- mapV :: V (R (a -> b) -> R [a] -> R [b])
-- mapV = undefined

-- TODO this doesn't seem right
hyGetFor1 :: (R a -> R b) -> (a -> b)
hyGetFor1 rf a =
  case rf (R a (error "whoopsie hyGetFor1")) of
    R b _ -> b
hyGetRev1 :: (R a -> R b) -> (R a -> b -> Write)
hyGetRev1 rf ra =
  let (R _ (Receiver _ rb)) = rf ra
   in rb

map_for :: (R a -> R b) -> [a] -> [b]
map_for rf as = map (hyGetFor1 rf) as
map_rev :: R (R a -> R b) -> R [a] -> [b] -> Write
map_rev (R rf _) (R oas (Receiver _ ras)) bs =
  let -- foo :: [a] -> [b] -> Write
      -- TODO not have to reverse this?
      foo nas [] [] = ras (reverse nas)
      foo nas (oa:oas) (b:bs) =
        let ra = R oa (Receiver "map_rev" cont)
            cont na = foo (na:nas) oas bs
         in rev ra b
      -- rev :: R a -> b -> Write
      rev = hyGetRev1 rf
   in foo [] oas bs
mapHy :: R (R a -> R b) -> R [a] -> R [b]
mapHy = hybrid2 map_for map_rev
mapV :: V (R (R a -> R b) -> R [a] -> R [b])
mapV = VConst "mapV" mapHy

compose_for :: (R b -> R c) -> (R a -> R b) -> (R a -> R c)
compose_for = (.)
composeHy :: R (R b -> R c) -> R (R a -> R b) -> R (R a -> R c)
composeHy = hybrid2 compose_for (no_rev "composeV")
composeV :: V (R (R b -> R c) -> R (R a -> R b) -> R (R a -> R c))
composeV = VConst "composeV" composeHy

-- -- External map
-- -- Just writing it in regular and primitive forms
-- mapE :: (a -> b) -> [a] -> [b]
-- mapE _ [] = []
-- mapE f (x:xs) = f x : (mapE f xs)
-- mapE' :: (a -> b) -> [a] -> [b]
-- mapE' f xs =
--   if null xs
--     then []
--     else (f (head xs)) : (map f (tail xs))

-- External
mapVE :: (Show a, Show b) => V (R a -> R b) -> V [a] -> V [b]
mapVE vf vas =
  ifV <**> (nullV <$$> vas)
      <**> (VConst "[]" [])
      <$$> (consV <**> (vf <$$> (headV <$$> vas))
                  <$$> (mapVE vf
                         (tailV <$$> vas)))

mapVE' :: (Show a, Show b) => V (R a -> b) -> V [a] -> V [b]
mapVE' vf vas =
  ifV <**> (nullV <$$> vas)
      <**> (VConst "[]" [])
      <$$> (consV <**> (vf <**> (headV <$$> vas))
                  <$$> (mapVE' vf
                         (tailV <$$> vas)))
  -- consV <**> (vf <**> (headV <$$> vas)) <$$> (VConst "[]" [])

zipWithV ::
  (Show a, Show b, Show c, Eq a, Eq b, Eq c) =>
  V (R a -> R b -> R c) -> V [a] -> V [b] -> V [c]
zipWithV vf vas vbs =
  ifV <**> (orV <**> (nullV <$$> vas)
                <$$> (nullV <$$> vbs))
      <**> (VCheckConst "zipWith" [])
      <$$> (consV <**> (vf <**> (headV <$$> vas) <$$> (headV <$$> vbs))
                  <$$> (zipWithV vf (tailV <$$> vas) (tailV <$$> vbs)))

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
fyold :: (a -> b -> b) -> b -> [a] -> b
fyold f b (a:as) = fyold f (f a b) as
fyold f b [] = b
fyold' f b as =
  if null as
    then b
    else fyold' f (f (head as) b) (tail as)

foldrVE :: (Show a, Show b) => V (R a -> R b -> R b) -> V b -> V [a] -> V b
foldrVE vf vb vas =
  ifV <**> (nullV <$$> vas)
      <**> vb
      <$$> (foldrVE vf
                    (vf <**> (headV <$$> vas) <$$> vb)
                    (tailV <$$> vas))

composo :: [a -> a] -> (a -> a)
composo [] = id
composo (f : fs) = composo fs . f

composo_for :: [R a -> R a] -> (R a -> R a)
composo_for = composo
composoHy :: R [R a -> R a] -> R (R a -> R a)
composoHy = hybrid1 composo_for (no_rev "composoV")
composoV :: V (R [R a -> R a] -> R (R a -> R a))
composoV = VConst "composoV" composoHy

-- TODO: why is reverseV needed here??
foldoVE :: (Show a, Show b) => V (R a -> R b -> R b) -> V b -> V [a] -> V b
foldoVE vf vb vas =
  (composoV <$$> (reverseV <$$> (mapVE' vf vas))) <$$> vb

foldo :: (a -> b -> b) -> b -> [a] -> b
foldo f b as = composo (map f as) b

mapViaFold :: (a -> b) -> [a] -> [b]
--mapViaFold f as = foldr (\a bs -> f a : bs) [] as
mapViaFold f as = foldr ((:) . f) [] as

mapViaFoldVE vf vas =
  let fooo = composeVE consV vf
   in foldoVE (VUnPartialApp fooo) (VCheckConst "mapViaFoldVE nil" []) vas

reverse_for :: [a] -> [a]
reverse_for = reverse
reverse_rev :: R [a] -> [a] -> Write
reverse_rev (R _ ra) as = ra <-- reverse as
reverseV :: V (R [a] -> R [a])
reverseV = VConst "reverseV" $ hybrid1 reverse_for reverse_rev

reverseVE :: (Eq a, Show a) => V [a] -> V [a]
reverseVE vas =
  ifV <**> (nullV <$$> vas)
      <**> (VConst "[]" [])
      <$$> (appendV <**> (reverseVE (tailV <$$> vas))
                    <$$> (consV <**> (headV <$$> vas)
                                <$$> (VCheckConst "reverseVE" [])))

reverseAcc :: [a] -> [a]
reverseAcc xs = reverseAcc' xs []
  where reverseAcc' [] xs' = xs'
        reverseAcc' (x:xs) xs' = reverseAcc' xs (x:xs')

reverseAccVE :: (Eq a, Show a) => V [a] -> V [a]
reverseAccVE xs = reverseAccVE' xs (VConst "reverseAccVE" [])
  where reverseAccVE' xs xs' =
          ifV <**> (nullV <$$> xs)
              <**> xs'
              <$$> (reverseAccVE' (tailV <$$> xs)
                                  (consV <**> (headV <$$> xs) <$$> xs'))

-- Matches sizes in reverse, so array cannot change length
append_for :: [a] -> [a] -> [a]
append_for = (++)
append_rev :: R [a] -> R [a] -> [a] -> Write
append_rev (R xs rxs) (R ys rys) zs = ((rxs <-- xs') <> (rys <-- ys'))
  where (xs', ys') = splitAs (length xs) (length ys) zs
        splitAs len0 len1 xs = assertM "appendV: length mismatch" ok (xs', xs'')
          where xs' = take len0 xs
                xs'' = drop len0 xs
                ok = length xs' + length xs'' == length xs
appendV :: V (R [a] -> R [a] -> R [a])
appendV = VConst "appendV" $ hybrid2 append_for append_rev

inxV :: V (R [a] -> R Int -> R a)
inxV = VConst "inxV" $ hybrid2 for rev
  where for = (!!)
        rev (R as ras) (R i _) a' = ras <-- (as !!- i) a'

fstV :: V (R (a, b) -> R a)
fstV = VConst "fstV" $ hybrid1 for rev
  where for = fst
        rev (R (a, b) rab) a' = rab <-- (a', b)

sndV :: V (R (a, b) -> R b)
sndV = VConst "sndV" $ hybrid1 for rev
  where for = snd
        rev (R (a, b) rab) b' = rab <-- (a, b')

-- mapVE :: (Show a, Show b) => V (R a -> R b) -> V [a] -> V [b]
-- reverseVE :: Show a => V [a] -> V [a]
-- reverseVE vas =
--   ifV <**> (nullV <$$> vas)
--       <**> (VConst "[]" [])
--       <$$> (consV <**> (vf <$$> (headV <$$> vas))
--                   <$$> (mapVE vf
--                          (tailV <$$> vas)))

-- -- mapViaFoldVE vf vas =
-- -- foo = composoV consV (shiftNAddV <$$> 10)
-- -- • Couldn't match type ‘R [b] -> R [b]’ with ‘R c’
-- --   Expected type: V (R b -> R c)
-- --     Actual type: V (R b -> R [b] -> R [b])

-- -- works
-- foo = composeVE consV (shiftNAddV <**> VConst "" 4)
-- bar = (composeVE consV (shiftNAddV <**> VConst "" 4) (VConst "" 12)) <**> VConst "" []
-- -- baz = foldoVE <**> (composeVE consV (shiftNAddV <**> VConst "" 4))
-- baz = foldoVE (VUnPartialApp foo) (VConst "" []) -- <**> (VConst "" [3, 4])

-- -- qqq :: V (R Int) -> V (R [Int]) -> V (R [Int])
-- -- qqq = \x -> (((foo . VSeal) x) <**>) . VSeal

-- composeVE vfbc vfab va = vfbc <$$> (vfab <$$> va)
-- composeVE vfbc vfab va = vfbc <**> (vfab <**> va)
composeVE :: (Show a, Show b) => V (R a -> rest) -> V (R b -> R a) -> V b -> V rest
composeVE vfbc vfab va = vfbc <**> (vfab <$$> va)

shiftNAdd :: R Int -> R Int -> R Int
shiftNAdd (R x rx) (R y ry) = R z rz
  where z = y * 10 + x
        rz = Receiver "shiftNAdd" $ \z' -> let x' = z' `mod` 10
                                               y' = z' `div` 10
                                            in (rx <-- x') <> (ry <-- y')
shiftNAddV :: V (R Int -> R Int -> R Int)
shiftNAddV = VConst "shiftNAddV" shiftNAdd

ifV_for :: Bool -> a -> a -> a
ifV_for b t e = if b then t else e
ifV_rev :: R Bool -> R a -> R a -> a -> Write
-- ifV_rev = error "ifV_rev"
-- ifV_rev _ _  _ _ = emptyWrite
ifV_rev (R b _rb) (R t rt) (R e re) x =
  let Receiver _ rec = if b then rt else re
   in rec x
ifV :: V (R Bool -> R a -> R a -> R a)
ifV = VConst "ifV" $ hybrid3 ifV_for ifV_rev

headR :: R [a] -> R a
headR (R as (Receiver _ ras)) = R a ra
  where a = head as
        ra = Receiver "headR" $ \a -> ras (a : tail as)
headV :: V (R [a] -> R a)
headV = VConst "headV" headR

tailR :: R [a] -> R [a]
tailR (R as (Receiver _ ras)) = R as' ras'
  where as' = tail as
        ras' = Receiver "tailR" $ \as' -> ras (head as : as')
tailV :: V (R [a] -> R [a])
tailV = VConst "tailV" tailR

consR :: R a -> R [a] -> R [a]
consR (R a ra) (R as ras) = R newAs ras'
  where newAs = a:as
        ras' = Receiver "consR" $ \(a':as') -> (ra <-- a') <> writeIfNotNil as'
        writeIfNotNil as' =
          if null as && null as'
            then emptyWrite
            else ras <-- as'
consV :: V (R a -> R [a] -> R [a])
consV = VConst "consV" consR

nullR :: R [a] -> R Bool
nullR = hybrid1 null undefined

nullV :: V (R [a] -> R Bool)
nullV = VConst "nullV" nullR

uni2 :: String -> (a -> b -> c) -> V (R a -> R b -> R c)
uni2 n f = VConst n (hybrid2 f (no_rev n))

bi2 :: String -> (a -> b -> c) -> (c -> (a, b)) -> V (R a -> R b -> R c)
bi2 name for rev = VConst name $ hybrid2 for rev'
  where rev' (R _ ra) (R _ rb) c =
          let (a, b) = rev c
           in (ra <-- a) <> (rb <-- b)

orV :: V (R Bool -> R Bool -> R Bool)
orV = uni2 "orV" (||)
-- orV = VConst "orV" (hybrid2 (||) (no_rev "orV"))

tup2 :: V (R a -> R b -> R (a, b))
-- tup2 = uni2 "tup2" (,)
tup2 = bi2 "tup2" (,) id

zipV = zipWithV tup2

-- (<**>) :: (Show a) => V (R a -> rest) -> V a -> V rest
modded = mapV <**> modStringV <$$> invitedUsersV

add_hy :: Int -> R Int -> R Int
add_hy n (R x rx) = R x' rx'
  where x' = x + n
        rx' = Receiver "inc_hy" $ \x ->
          rx <-- x - n
addV :: Int -> V (R Int -> R Int)
addV n = VConst "addV" $ add_hy n

-- inc_hy :: R Int -> R Int
-- inc_hy (R x rx) = R x' rx'
--   where x' = x + 1
--         rx' = Receiver "inc_hy" $ \x ->
--           rx <-- x - 1
-- incV :: V (R Int -> R Int)
-- incV = VConst "incV" inc_hy
incV = addV 1

plus_for :: Int -> Int -> Int
plus_for = (+)
plus_rev :: R Int -> R Int -> Int -> Write
plus_rev (R x rx) (R y ry) newZ =
  (rx <-- x') <>
  (ry <-- y')
  where x' = newZ `div` 2
        y' = newZ - x'
plus_hy' :: R Int -> R Int -> R Int
plus_hy' = hybrid2 plus_for plus_rev
plusV = VConst "plusV" plus_hy'

newtype Ext a = Ext (IO a)

data Initiation = Initiation ExecId deriving Show

data Req = Req String deriving Show
data Resp = Resp String deriving Show

data Rpc = Rpc
  { calls :: [Call]
  , toExt :: Req -> Ext Resp
  , toTmi :: Resp -> TMI W ()
  }
_calls = mkFielder "_calls" calls $ \w a -> w { calls = a }

instance Show Rpc where
  -- TODO do not love this
  show rpc = "RPC " ++ show (calls rpc)

initRpc :: Rpc
initRpc = Rpc [] toExt toTmi
  where toExt (Req s) = Ext io
          where io = do
                  msp $ "Running ext: " ++ s
                  return $ Resp $ s ++ "!"
        toTmi (Resp s) = do
          liftIO $ msp $ "Consequence: " ++ s

data Call = Call
  { req :: Req
  , intitiation :: Maybe Initiation
  , resp :: Maybe Resp
  , consquenceEnacted :: Bool
  } deriving Show

initCall :: Req -> Call
initCall req = Call req Nothing Nothing False

refreshRpcs :: W -> IO W
refreshRpcs w = do
  -- Clear out stale initiations
  -- Get requests that need initiation
  -- Initiate them
  -- Write initiations to table
  return w

-- eventLoop :: History W -> IO ()
-- eventLoop h = do
--   h' <- tmiRunIO h refreshRpcs
--   msp $ h'
--   return ()

extAction :: TMI W ()
extAction = do
  listen (_aList <$$> vw) listeny
  let rpc = _rpc <$$> vw
      calls = _calls <$$> rpc
  let call = initCall (Req "hey")
  calls <--- appendV <**> calls <$$> VConst "" [call]

action :: TMI W ()
action = do
  -- TODO we shouldn't change history in an action, and also it's ignored, so
  -- this doesn't work
  listen invitedUsersV listeny
  listen modded listeny
  -- listen (ifV <**> VConst True <**> VConst 2 <$$> VConst 3) listeny
  -- listen (ifV <**> VConst False <**> VConst 2 <$$> VConst 3) listeny
  -- listen (headV <$$> VConst [3, 4, 5]) listeny
  -- listen (tailV <$$> VConst [3, 4, 5]) listeny
  listen (_aList <$$> vw) listeny
  listen (_anotherList <$$> vw) listeny
  listen (_zero <$$> vw) listeny
  -- listen (headV <$$> (_aList <$$> vw)) listeny
  -- listen (tailV <$$> (_aList <$$> vw)) listeny
  -- listen (consV <**> (headV <$$> (_aList <$$> vw))
  --               <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) listeny
  -- let mappuh = composeV <**> incV <$$> (addV 4) -- works
  let mappuh = composoV <$$> incers
      incers = consV <**> incV <$$> (consV <**> (addV 5) <$$> (VConst "[]" []))
  let mapped = mapVE mappuh (_aList <$$> vw)
  let aFold :: V Int
      aFold = foldrVE shiftNAddV (_zero <$$> vw) (_anotherList <$$> vw)
  listen mapped listeny
  listen aFold listeny
  let aFoldo :: V Int
      aFoldo = foldoVE shiftNAddV (_zero <$$> vw) (_anotherList <$$> vw)
  listen aFoldo listeny
  -- let fooo = composeVE consV (shiftNAddV <**> VConst "" 4)
  --     mappedViaFold = foldoVE (VUnPartialApp fooo) (VConst "" []) (_aList <$$> vw)
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
  let inxed = inxV <**> (_aList <$$> vw) <$$> (VConst "" 1)
  listen inxed listeny
  let firsty = fstV <$$> (inxV <**> zippie2 <$$> VConst "" 1) 
  listen firsty listeny
  let secondy = sndV <$$> (inxV <**> zippie2 <$$> VConst "" 1) 
  listen secondy listeny
  let appo = _anAppendo <$$> vw
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
  -- mapped <--- VConst "" [302, 402, 502] -- works
  -- mappedViaFold <--- VConst "" [5964,6964,7964] -- works
  -- reverseMVF <--- VConst "" [7964,6964,5964]
  -- aFold <--- VConst "" (456::Int) -- Works
  -- aFoldo <--- VConst "" (789::Int) -- Works
  -- (headV <$$> (_aList <$$> vw)) <--- VConst 31
  -- (tailV <$$> (_aList <$$> vw)) <--- VConst [42, 52]
  -- Non-singular write
  -- (consV <**> (headV <$$> (_aList <$$> vw))
  --        <$$> (tailV <$$> (tailV <$$> (_aList <$$>) vw))) <--- VConst [310, 520]

extMain = do
  (a, history') <- tmiRun history action
  -- (a, history') <- tmiRun history extAction
  -- eventLoop history'
  -- history'' <- tmiRunIO history' refresh

  -- msp $ fyold (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ fyold' (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ foldo (\x acc -> acc * 10 + x) 0 [2::Int, 3, 4]
  -- msp $ mapViaFold (+1) [4, 5, 6]
  -- msp $ reverseAcc [5, 6, 7]
  msp $ "result " ++ show a
  -- msp history'
  -- runListeners history'
  -- msp $ case history' of History _ listeners -> length listeners
  -- msp $ map (+1) [3, 4, 5]
  -- msp $ mapE (+1) [3, 4, 5]
  -- msp $ mapE' (+1) [3, 4, 5]
  msp "ext hi"

-- $> :t invitedUsersV
