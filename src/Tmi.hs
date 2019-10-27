{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Tmi
( Val(..)
, (<--)
, oldWebDemo
, deltaTmiDemo
, bankProcess) where

import Control.Applicative
import Control.Monad.State
import qualified Data.CaseInsensitive as CI
import Data.Function
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Debug.Trace as TR
import Network.HTTP.Types.Status (ok200, found302)
import Network.URI.Encode as ENC
import System.Directory (copyFile)
import System.IO
import Text.Pretty.Simple (pShow)
import Web.Firefly

import Util 

type History = [DB]
data NN = NN { nnn :: [Int] }
  deriving (Eq, Read, Show)
data DB = DB { a :: Int, b :: [Int], bb :: [Int], c :: String, accounts :: M.Map String Int,
               nn :: NN }
  deriving (Eq, Read, Show)

thedb = DB { a = 12, b = [2, 3, 4], bb = [20, 30, 40], c = "asdf", accounts = M.fromList [],
             nn = NN { nnn = [4, 5, 6] } }

data Val b = Val (DB -> b) (b -> DB -> DB)

type a --> b = Val a -> Val b
type a ---> b = Val a -> b

goo :: String --> Int
goo (Val _ _) = vconst 3
hoo :: String ---> (Int --> Float)
hoo (Val _ _) (Val _ _) = vconst 3.4

vconst v = uni $ const v

for (Val f r) = f
rev (Val f r) = r

norev = error "norev"

uni f = Val f norev

vread = for

theroot = Val id const

liftV :: (a -> b) -> Val a -> Val b
liftV f = liftBV f norev
liftV2 :: (a -> b -> c) -> Val a -> Val b -> Val c
liftV2 f = liftBV2 f norev
liftBV :: (a -> b) -> (b -> a -> a) -> Val a -> Val b
liftBV f b x = Val nf nb
  where nf db = f (for x db)
        nb ny db = rev x (b ny (for x db)) db
liftBV2 :: (a -> b -> c) -> (c -> (a, b) -> (a, b)) -> Val a -> Val b -> Val c
liftBV2 f b bbb ccc = Val fd bd
  where fd x = f (for bbb x) (for ccc x)
        bd nv x = let (nb, nc) = b nv (for bbb x, for ccc x)
                   in rev ccc nc (rev bbb nb x)

vsp v = msp $ vread v thedb

vwrite :: Val a -> Val a -> DB -> DB
vwrite (Val f b) v a = b (vread v a) a

-- bidi inc
binc :: Val Int -> Val Int
binc = liftBV (+1) (\i _ -> i-1)

-- Bidirectional additition: in the reverse direction, spread the change
-- between the two inputs.  So forward 1 + 1 = 2 ; reverse 4 = 2 + 2
bidiPlus :: Val Int -> Val Int -> Val Int
bidiPlus = liftBV2 (\x y -> x + y) rev
  where rev nsum (ox, oy) = (nx, ny)
          where osum = ox + oy
                delta = nsum - osum
                nx = ox + (delta `div` 2)
                ny = nsum - nx

instance Num a => Num (Val a) where
  (+) = liftV2 (+)
  (*) = liftV2 (*)
  abs = liftV abs
  signum = liftV signum
  fromInteger i = uni $ const $ fromInteger i
  negate = liftV negate

instance IsString a => IsString (Val a) where
  fromString s = uni $ const $ fromString s

ntrue = vconst True
nfalse = vconst False

nif :: Val Bool -> Val b -> Val b -> Val b
nif c ~t ~e = uni f
  where f db = if (for c db) then (for t db) else (for e db)

neq :: Eq b => Val b -> Val b -> Val Bool
neq = liftV2 (==)

nhead :: Val [b] -> Val b
nhead = liftBV head $ \x (_:xs) -> x:xs

ntail :: Val [b] -> Val [b]
ntail = liftBV tail $ \xs (x:_) -> x:xs

ncons :: Val b -> Val [b] -> Val [b]
ncons = liftBV2 (:) $ \(x:xs) _ -> (x, xs)

nmap :: (Eq a, Show a) => (Val a -> Val b) -> Val [a] -> Val [b]
nmap f as = nif (neq as (vconst []))
                (vconst [])
                (ncons (f (nhead as)) (nmap f (ntail as)))

nmap2 = liftV2 map

nmain = do
  hSetBuffering stdin NoBuffering
  msp "hi"
  msp $ vread theroot thedb
  --vsp theroot
  vsp $ _a
  vsp $ (liftV (+ 10)) $ _a
  vsp $ (liftV2 (+)) _a (_bi 1)
  msp $ vwrite _a 120 thedb
  massert $ (vwrite _a 120 thedb) == DB { a = 120 , b = [ 2 , 3 , 4 ] , bb = [20, 30, 40], c = "asdf", accounts = M.fromList [], nn = NN { nnn = [1] } } 
  vsp $ binc $ _a
  massert $ (vwrite (binc $ _a) 130 thedb) ==
    DB { a = 129 , b = [ 2 , 3 , 4 ] , bb = [20, 30, 40], c = "asdf", accounts = M.fromList [], nn = NN { nnn = [1] } } 
  vsp $ _a `bidiPlus` (_bi 1)
  msp $ vwrite (_a `bidiPlus` (_bi 1)) 19 thedb
  massert $ (vwrite (_a `bidiPlus` (_bi 1)) 19 thedb) ==
    DB { a = 14 , b = [ 2 , 5 , 4 ] , bb = [20, 30, 40], c = "asdf", accounts = M.fromList [], nn = NN { nnn = [1] } }
  let floo :: Val Int
      floo = 123
  vsp floo
  msp "mult"
  msp $ vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb
  massert $ (vwrite (_bi 1) 335 $ vwrite _a 126 $ vwrite _c "zxcv" thedb) ==
    DB { a = 126 , b = [ 2 , 335 , 4 ] , bb = [20, 30, 40], c = "zxcv", accounts = M.fromList [], nn = NN { nnn = [1] } }
  vsp $ nmap (liftV (\x -> x * 2)) (vconst [1, 2, 3])
  vsp $ nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])
  massert $ (vread (nmap (liftV (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
  massert $ (vread (nmap2 (vconst (\x -> x * 2)) (vconst [1, 2, 3])) thedb) == [2, 4, 6]
  vsp $ floo `neq` 120
  vsp $ floo `neq` 123
  vsp $ nif (vconst True) "istrue" "isfalse"
  vsp $ nif (vconst False) "istrue" "isfalse"
  vsp $ nif ntrue "istrue" "isfalse"
  vsp $ nif nfalse "istrue" "isfalse"
  vsp $ neq ntrue ntrue
  vsp $ neq ntrue nfalse
  vsp $ neq 12 12
  vsp $ neq 12 13
  vsp $ liftV (*10) 13
  vsp $ nhead (vconst [20, 21, 22])
  vsp $ ntail (vconst [20, 21, 22])
  vsp $ nhead $ ntail (vconst [20, 21, 22])
  vsp $ ncons 19 (vconst [20, 21, 22])
  vsp $ ncons 19 $ ntail (vconst [20, 21, 22])
  vsp $ (+ 10) _a
  vsp $ (+) _a (_bi 1)
  vsp $ _a + 10
  vsp $ 10 + _a
  vsp $ _a + (_bi 1)
  vsp $ nhead _b
  msp $ vwrite (nhead _b) 20 thedb
  vsp $ ntail _b
  msp $ vwrite (ntail _b) (vconst [30, 40]) thedb
  vsp $ ncons _a _b
  msp $ vwrite (ncons _a _b) (vconst [1200, 200, 300, 400]) thedb

up_a v db = db { a = v }
up_b v db = db { b = v }
up_bb v db = db { bb = v }
up_c v db = db { c = v }
up_accounts :: M.Map String Int -> DB -> DB
up_accounts v db = db { accounts = v }
up_nn v db = db { nn = v }
up_nnn v nn = nn { nnn = v }
_a :: Val Int
_a = liftBV a up_a theroot
_b = liftBV b up_b theroot
_bb = liftBV bb up_bb theroot
_c = liftBV c up_c theroot
_accounts = liftBV accounts up_accounts theroot
_i :: Int -> Val [a] -> Val a
_i i = liftBV (!! i) (\nv oarr -> upd oarr i nv)
_m_f :: Ord a => a -> M.Map a b -> b
_m_f k m = m M.! k
_m_b :: Ord a => a -> b -> M.Map a b -> M.Map a b
_m_b k v m = M.insert k v m
_m k = liftBV (_m_f k) (_m_b k)
_keys = liftV M.keys
upd :: [a] -> Int -> a -> [a]
upd as i a
  | i < 0 || i >= length as = error "upd out of range"
  | otherwise = (take i as) ++ [a] ++ (drop (i+1) as)
--_bi i = uni $ \arr -> b arr !! i
_bi :: Int -> Val Int
_bi i = (_i i) _b

type Write = DB -> DB
mkwrite :: Val a -> Val a -> Write
mkwrite = vwrite

type TMI a = StateT [Write] IO (Val a)

infix 4 <--
(<--) :: Val a -> Val a -> TMI ()
dest <-- src = do
  writes <- get
  put $ writes ++ [mkwrite dest src]
  return $ vconst ()

applyWrites :: [Write] -> DB -> DB
applyWrites writes db = foldl (&) db writes

tmiRun :: DB -> TMI a -> IO (a, DB)
tmiRun db action = do
  (x, writes) <- runStateT action []
  let result = vread x db
      newDb = applyWrites writes db
  --msp newDb
  return (result, newDb)

persistentRun :: TMI a -> IO a
persistentRun action = do
  historyS <- readFile "history.db"
  let history :: History
      history = (read historyS) :: History
  (result, newDb) <- tmiRun (last history) action
  let newHistory = history ++ [newDb]
      newHistoryS = sp newHistory
  writeFile "history.db" newHistoryS
  return result

class Delta a d | d -> a where
  (.+) :: a -> d -> a
  (.-) :: a -> a -> d

data ListDelta a = Insert Int a | Delete Int
deriving instance Show a => Show (ListDelta a)

instance Delta [a] (ListDelta a) where
  xs .+ (Insert i x) = (take i xs) ++ [x] ++ (drop i xs)
  (.-) = undefined  -- slow

data DBDelta = DBDeltaB (ListDelta Int) | DBDeltaBB (ListDelta Int)

instance Delta DB DBDelta where
  db .+ (DBDeltaB ld) = up_b ((b db) .+ ld) db
  (.-) = undefined  -- slow

-- This is wrong, of course
instance Delta a da => Delta a [da] where
  db .+ (da : das) = (db .+ da) .+ das
  db .+ [] = db
  (.-) = undefined  -- slow

instance (Delta a da, Delta a da') => Delta a (Either da da') where
  db .+ (Left da) = db .+ da
  db .+ (Right da') = db .+ da'
  (.-) = undefined  -- slow

-- Turn a list delta into a db delta
fyoo :: ListDelta Int -> DBDelta
fyoo = DBDeltaB

-- data Val b = Val (DB -> b) (b -> DB -> DB)
-- b :: DB -> [Int]
-- up_b :: [Int] -> DB -> DB
-- up_b v db = db { b = v }
-- _b = liftBV b up_b theroot
-- DBWriteDeltaB (Insert 2 4)

-- Is the constaint necessary?
--data Viff b = forall db . Delta b db => Viff (DB -> b) (b -> DB -> DB)
--data Viff b = Viff (DB -> b) (b -> DB -> DB)

{-
ggoo :: forall da . Delta a da -> (a -> a)
ggoo = undefined
liftVF :: forall da db . (Delta a da, Delta b db) => (a -> b) -> (b -> a -> a) -> Viff a -> Viff b
liftVF = undefined

liftDV :: (Delta a da, Delta b db) => (a -> b) -> (b -> a -> a) -> (da -> db) -> (db -> a -> da) -> DVal a da -> DVal b db
liftBDV f b df db (DVal afor arev adrev) = DVal bfor brev bdrev
  where --bfor :: DB -> b
        bfor w = f (afor w)
        --brev :: b -> DB -> DB
        brev x w = arev (b x (afor w)) w
        --bdrev :: db -> DB -> DB
        bdrev dx w = arev ((afor w) .+ (db dx (afor w))) w
-}

vfnn :: Val NN
vfnn = Val nn up_nn

vfnnn :: Val [Int]
--vfnnn = Val nnn up_nnn
vfnnn = (liftBV nnn up_nnn) vfnn

----type VFun a b = Val a -> Val b

----class VFun a b

--data BFun a b = BFun (a -> b) (b -> a -> a)

--class VFun f

--data DMap a b = DMap (BFun a b)
--instance VFun (DMap a b)
--data DReverse
--instance VFun DReverse

---- TODO works with just db -> da?  Should...
--class (Delta a da, Delta b db, VFun vf) => Differ vf a b da db | vf a b db -> da where
--  differ :: vf -> db -> a -> da

---- Super not crazy about the list lookup here, we can omit it because we know map doesn't need it?
--instance Differ (DMap a b) [a] [b] (ListDelta a) (ListDelta b) where
--  differ (DMap (BFun for rev)) (Insert i b) as = Insert i (rev b (as !! i))

---- TODO more idiomatic way to ignore the second param
--double :: DMap Int Int
--double = DMap (BFun (* 2) (\x _ -> x `div` 2))

---- TODO more idiomatic way to ignore the second param
--addone :: DMap Int Int
--addone = DMap (BFun (+ 1) (\x _ -> x - 1))

--data ComposeDiffers dfa dfb = dfa :..: dfb

----{-
----ARGH

----na :: Val ? a
----fab :: Fun a b
----nb :: Val ? b
----nb = fab `app` na
----app :: Val x a -> Fun a b -> Val (Fun a b) b

----db :: Val (Fun () DB) DB
----getb :: Fun DB a
----na :: Val (Fun DB a) a
----na = getb `app` db
----fab = Fun a b
----nb = fab `app` na
----nb :: Val (Fun a b) b
----app :: Fun a b -> Val c a -> Val (Fun a b) a
-----}

data VFun a b = VFun (a -> b) (b -> a -> a)

--data Bree a b = Bree (VFun a b) b
data Bree a b = Bree (VFun a b) (World -> b) (b -> World -> World)
--data Bree a b = Bree (VFun a b) b

--vapply :: VFun a b -> Bree vf a -> Bree vf b
--vapply :: VFun a b -> Bree c a -> Bree a b
--vapply (VFun vf) (Bree ovf a) = Bree (VFun vf) (vf a)

data Blerb = Blerb { wba :: [Int] } deriving Show
data World = World { wa :: [Int], wb :: Blerb } deriving Show
worldData :: World
worldData = World { wa = [1, 2, 3], wb = Blerb { wba = [4, 5, 6] } }
u_wa wa w = w { wa = wa }
u_wb wb w = w { wb = wb }
u_wba wba w = w { wba = wba }

world :: Bree World World
world = Bree (VFun f r) id (\w _ -> w) -- TODO const?
  where f _ = worldData
        r w _ = w

--nwa :: Bree World [Int]
--nwa = Bree (VFun wa) (wa worldData)

vapply :: VFun a b -> Bree c a -> Bree a b
vapply (VFun vf vr) (Bree _ w2a a2w) = Bree (VFun vf vr) w2b b2w
  where w2b w = vf $ w2a w
        b2w b w = a2w a w
          where a = vr b (w2a w)

vvread :: Bree a b -> World -> b
vvread (Bree _ w2b _) w = w2b w

vvwrite :: Bree a b -> b -> World -> World
vvwrite (Bree vf f r) b w = r b w

nwa = vapply (VFun wa u_wa) world
nwb = vapply (VFun wb u_wb) world
nwba = vapply (VFun wba u_wba) nwb
nwbai i = vapply (VFun (!! i) rev) nwba
  where rev b arr = upd arr i b

vmap' :: (a -> b) -> (b -> a -> a) -> VFun [a] [b]
vmap' f r = VFun (map f) blah
  where -- blah :: [b] -> [a] -> [a]
        blah bs as = map (\(b, a) -> r b a) (zip bs as)
vmap :: (a -> b) -> (b -> a -> a ) -> VMap a b
vmap f r = VMap (f, r, (vmap' f r))

--data VFun a b = VFun (a -> b) (b -> a -> a)
newtype VMap a b = VMap ((a -> b), (b -> a -> a), (VFun [a] [b]))
instance Wrapper (VMap a b)

class Wrapper a

class (Delta a da, Delta b db, Wrapper wr) => Incremental a b da db wr where
  applyDelta :: wr -> db -> a -> da

instance Incremental [a] [b] (ListDelta a) (ListDelta b) (VMap a b) where
  applyDelta (VMap (f, r, _)) (Insert i bb) as = (Insert i aa)
    where aa = r bb (as !! i)
  applyDelta (VMap (f, r, _)) (Delete i) _ = (Delete i)

data ConsDelta a = Cons a | Snoc a
deriving instance Show a => Show (ConsDelta a)

instance Delta [a] (ConsDelta a) where
  xs .+ (Cons x) = x : xs
  xs .+ (Snoc x) = xs ++ [x]
  (.-) = undefined  -- slow

instance Incremental [a] [b] (ConsDelta a) (ConsDelta b) (VMap a b) where
  applyDelta (VMap (f, r, _)) (Cons bb) as = (Cons aa)
    where aa = r bb (head as)
  applyDelta (VMap (f, r, _)) (Snoc bb) as = (Snoc aa)
    where aa = r bb (head as)

instance Incremental a b da db wr => Incremental a b [da] [db] wr where
  applyDelta wr dbs a = map (\db -> applyDelta wr db a) dbs

data NullDelta a = NullDelta
  deriving (Eq, Show)

instance Delta a (NullDelta a) where
  x .+ NullDelta = x
  (.-) = undefined  -- TODO: maybe assert args are equal? Maybe (.-) should be maybe?

instance Incremental a b (NullDelta a) (NullDelta b) (VFun a b) where
  applyDelta (VFun f r) NullDelta _ = NullDelta  -- TODO: or you could run it and assert it hasn't changed, see NullDelta

newtype FullDelta a = FullDelta a
  deriving (Eq, Show)

instance Delta a (FullDelta a) where
  x .+ FullDelta x' = x'
  (.-) = undefined -- TODO: Or is it just: x .- x' = x

-- map-specific, nope
--instance (Delta a (FullDelta a), Delta b (FullDelta b), Wrapper (VMap a b)) => Incremental [a] [b] (FullDelta [a]) (FullDelta [b]) (VMap a b) where
--  applyDelta (VMap (f, r, _)) (FullDelta bs) as = FullDelta $ map r bs as

-- not map-specific
--instance (Delta a a, Delta b b, Wrapper (VFun a b)) => Incremental a b (FullDelta a) (FullDelta b) (VFun a b) where
--instance Wrapper (VFun a b) => Incremental a b (FullDelta a) (FullDelta b) (VFun a b) where
instance Incremental a b (FullDelta a) (FullDelta b) (VFun a b) where
  applyDelta (VFun f r) (FullDelta b) a = FullDelta $ r b a
--applyDelta :: wr -> db -> a -> da

instance Wrapper (VFun a b)

--data IndividualDeltas da = IndividualDeltas [da]

--instance (Delta [a] (IndividualDeltas da), Delta [b] (IndividualDeltas db)) => Incremental [a] [b] (IndividualDeltas da) (IndividualDeltas db) (VMap a b) where
--  --applyDelta (VMap (f, r, _)) (IndividualDeltas dbs) as = IndividualDeltas (map (\(db, a) -> r db a) (zip dbs as))
--  --applyDelta vm (IndividualDeltas dbs) as = IndividualDeltas (map (\(db, a) -> applyDelta vm db a) (zip dbs as))
--  applyDelta   (IndividualDeltas dbs) as = IndividualDeltas (map (\(db, a) -> applyDelta vm db a) (zip dbs as))

newtype ParListDelta a = ParListDelta [a]
  deriving (Eq, Show)

instance Delta a da => Delta [a] (ParListDelta da) where
  as .+ (ParListDelta das) = map (\(a, da) -> a .+ da) (zip as das)
  as .- as' = ParListDelta $ map (\(a, a') -> a .- a') (zip as as')

--instance (Delta a da, Delta b db
instance Incremental a b da db wr => Incremental [a] [b] (ParListDelta da) (ParListDelta db) wr where
  applyDelta wr (ParListDelta dbs) as = ParListDelta $ map (\(db, a) -> applyDelta wr db a) (zip dbs as)
  --applyDelta wr dbs a = map (\db -> applyDelta wr db a) dbs

-- composing applyDelta
-- TODO this only works because none of our delta appliers use the second argument
-- Also, this doesn't work because there's no way to determine what type b is in the where clauses
-- More specifically, wbc and wab do not have any type variables, so they cannot be unified with the
-- input and output types. A specific instantion does have those, but perhaps that is masked by them
-- not being part of the Wrapper here.  But I can't figure out how to add them to Wrapper.
data ComposeWrappers bc ab = ComposeWrappers bc ab
instance (Wrapper ab, Wrapper bc) => Wrapper (ComposeWrappers bc ab)
instance (Incremental b c db dc wbc, Incremental a b da db wab) => Incremental a c da dc (ComposeWrappers wbc wab) where
  applyDelta (ComposeWrappers wbc wab) dc a = da
    where db :: db
          db = applyDelta wbc dc b
          b :: b
          b = undefined -- TODO never used anywhere, but might be
          da :: da
          da = applyDelta wab db a

-- class Wrapper2 a b
-- data VMap2 a b =
-- class (Delta a da, Delta b db, Wrapper wr) => Incremental a b da db wr where
--   applyDelta :: wr -> db -> da

-- Double -> Integer -> String
vmdi :: VMap Double Integer
vmdi = vmap floor (\i _ -> (fromInteger i) :: Double)
vmis :: VMap Integer String
vmis = vmap show (\s _ -> read s)
compositeFailureDemo = do
  msp $ ((applyDelta vmdi (Insert 1 (20::Integer)) [1.0::Double, 2.0, 3.0]) :: ListDelta Double)
  msp $ ((applyDelta vmis (Insert 1 ("20"::String)) [1::Integer, 2, 3]) :: ListDelta Integer)
  --msp $ ((applyDelta (ComposeWrappers vmis vmdi) (Insert 1 ("20"::String)) [1.0::Double, 2.0, 3.0]) :: ListDelta Double)

-- Function abstraction
data Brap a b = Brap (a -> b) (b -> a -> a)
bmap :: Brap a b -> BMap (Brap a b) (Brap [a] [b])
bmap (Brap f r) = BMap (Brap f r) (Brap (map f) rev)
  where rev bs as = map (\(b, a) -> r b a) (zip bs as)

class Guff a
data BMap f br = BMap f br
instance Guff (BMap f br)

-- Wrapped as singleton
gbincr :: BMap (Brap Int Int) (Brap [Int] [Int])
--gbincr = BMap bincr (bmap bincr)
gbincr = bmap bincr

class (Delta a da, Delta b db, Guff g) => Inc a b da db g where
  appInc :: g -> db -> a -> da

--instance (Delta a da, Delta b db) => Inc a b da db (BMap (Brap a b)) where
instance Inc [a] [b] (ListDelta a) (ListDelta b) (BMap (Brap a b) (Brap [a] [b])) where
  -- TODO: could choose to not lookup in as since it's not used; or a different form for this
  appInc (BMap (Brap f r) _) (Insert i b) as = Insert i (r b a)
    where a = as !! i

bincr = Brap (+1) (\n _ -> n - 1)
bapplyf (Brap f r) = f
bapplyr (Brap f r) = r

deltaTmiDemo = do
  msp $ ((appInc gbincr (Insert 1 (21::Int)) [11::Int, 31, 41]) :: (ListDelta Int))
  -- msp $ bapplyf bincr 3
  -- msp $ bapplyr bincr 4 3
  -- msp $ bapplyf (bmap bincr) [1, 2 ,3]
  -- msp $ bapplyr (bmap bincr) [2, 3, 4] [1, 2 ,3]
  -- --msp $ bapplyr (bmap bincr) [2, 3, 4] undefined  -- wish this worked
  msp "hi"

xx2 = vmap (* (2::Int)) (\x _ -> x `div` (2::Int))
pp1 = vmap (+ (1::Int)) (\x _ -> x - (1::Int))
deltaTmiDemo4 = do
  msp $ vvread world worldData
  msp $ vvread nwa worldData
  msp $ vvread nwb worldData
  msp $ vvread nwba worldData
  msp $ vvread (nwbai 1) worldData
  msp $ vvwrite nwa [10, 20, 30] worldData
  msp $ vvwrite nwba [40, 50, 60] worldData
  msp $ vvwrite (nwbai 2) 60 worldData
  let x2 = vmap (* (2::Int)) (\x _ -> x `div` (2::Int))
  let (VMap (_, _, huh)) = x2
  let p1 = vmap (+ (1::Int)) (\x _ -> x - (1::Int))
  msp $ ((applyDelta x2 (Insert 1 (20::Int)) [(1::Int), 2, 3]) :: (ListDelta Int))
  msp $ ((applyDelta x2 ((Delete 1) :: ListDelta Int) [(1::Int), 2, 3]) :: (ListDelta Int))
  msp $ ((applyDelta x2 (Cons (20::Int)) [(1::Int), 2, 3]) :: (ConsDelta Int))
  msp $ ((applyDelta x2 (Snoc (20::Int)) [(1::Int), 2, 3]) :: (ConsDelta Int))
  msp $ ((applyDelta x2 [Cons (20::Int), Cons (22::Int)] [(1::Int), 2, 3]) :: [(ConsDelta Int)])
  msp $ ((applyDelta huh (FullDelta [6, 4, (2::Int)]) [(2::Int), 4, 6]) :: (FullDelta [Int]))
  msp $ ((applyDelta huh (NullDelta :: (NullDelta [Int])) [(1::Int), 2, 3]) :: (NullDelta [Int]))
  msp $ ([[1, 2], [3, 4], [5, 6]] .+ ParListDelta [Insert 0 (11::Int), Insert 1 (44::Int), Insert 2 (77::Int)])
  msp $ ((applyDelta x2 (ParListDelta [Insert 0 (20::Int), Insert 1 (40::Int)]) [[(2::Int), 4], [(6::Int), 8]]) :: ParListDelta (ListDelta Int))
  -- Cannot determine the intermediate type from this, the type of b/db
  --msp $ ((applyDelta (ComposeWrappers p1 x2) (Insert 1 (41::Int)) [(3::Int), 5, 7]) :: ListDelta Int)
  msp "hi"
  --msp $ differ double (Insert 1 (20::Int)) (b thedb)
  --msp $ differ addone (Insert 1 (20::Int)) (b thedb)

  --msp $ dvwrite (NNDeltaNNN (Insert 2 (50 :: Int))) thedb
  --msp $ dvwrite' vfnn (NNDeltaNNN (Insert 2 (51 :: Int))) thedb
  --msp $ dvwrite' vfnnn (Insert 2 (52 :: Int)) thedb
  --msp $ vread vfnnn thedb
  --tmiRunShow hahaNN
  --tmiRunShow hahaNNN

{-
data Loo a = forall vf da . (VFun vf, Delta a da) => Loo a vf (da -> String)

class (VFun vf, Delta a da) => Chew vf a da where
  brap :: vf -> a -> da -> String

instance Delta b db => Chew (DMap a b) b db where
  brap _ _ db = "dmapab"

garsh :: (Chew vf a da, Delta a da) => Loo a -> da -> String
garsh (Loo a vf dfwut) da = brap vf a da
-}

--class (VFun vf, Delta a da) => forall vf. forall dc . Ruh a
--class forall a . Num a => Loo

  {-
instance (Differ fbc b c db dc, Differ fab a b da db) => Differ (ComposeDiffers fbc fab) a c da dc where
  --differ :: VFun -> dc -> a -> da
  differ (fbc :..: fab) dc a = fab (fbc dc b) a
    where b = ???
-}

  {-
class DViffer b db | db -> b where
  dvwrite :: db -> DB -> DB

instance DViffer NN NNDelta where
  dvwrite dx w = up_nn (nn w .+ dx) w
-}

class DViffer' b db | db -> b where
  dvwrite' :: Val b -> db -> DB -> DB

--instance DViffer' NN NNDelta where
  --dvwrite' (Val for rev) dx w = rev (for w .+ dx) w

instance Delta a da => DViffer' a da where
  dvwrite' (Val for rev) dx w = rev (for w .+ dx) w

(<--..) :: Delta a da => Val a -> da -> TMI()
dest <--.. srcDelta = do
  writes <- get
  let newWrite = dvwrite' dest srcDelta
  put $ writes ++ [newWrite]
  return $ vconst ()

--tmiRun :: DB -> TMI a -> IO (a, DB)
tmiRunShow :: TMI () -> IO ()
tmiRunShow action = do
  msp "before"
  msp thedb
  ((), newDb) <- tmiRun thedb action
  msp "after"
  msp newDb

hahaNN :: TMI ()
hahaNN = do
  vfnn <--.. (NNDeltaNNN (Insert 2 (51 :: Int)))
  return $ vconst ()

hahaNNN :: TMI ()
hahaNNN = do
  vfnnn <--.. Insert 2 (52 :: Int)
  return $ vconst ()

data Biff b = forall db . Delta b db => Biff (DB -> b) (b -> DB -> DB) (db -> DB -> DB)

bfnn :: forall db . (Delta NN db, Cbfnn db) => Biff NN
--bfnn = undefined
bfnn = Biff nn up_nn (dup_nn :: NNDelta -> DB -> DB)
--bfnn = Biff nn up_nn (dup_nn :: (Delta NN db, Cbfnn db) => db -> DB -> DB)

class Delta NN dnn => Cbfnn dnn where
  dup_nn :: Delta NN dnn => dnn -> DB -> DB

instance Cbfnn NNDelta where
  dup_nn dx w = up_nn (nn w .+ dx) w

data DVal b db = Delta b db => DVal (DB -> b) (b -> DB -> DB) (db -> DB -> DB)
-- _deltaB is an incremental lens view of DB.b
_deltaB :: DVal [Int] (ListDelta Int)
_deltaB = DVal b up_b up_db
  where up_db :: ListDelta Int -> DB -> DB
        up_db deltaB db = up_b newArr db
          where newArr = (b db) .+ deltaB
_deltaBB :: DVal [Int] (ListDelta Int)
-- y not
--_deltaBB :: Delta [Int] d => DVal [Int] d
_deltaBB = DVal bb up_bb up_dbb
  where up_dbb :: ListDelta Int -> DB -> DB
  -- y not
  --where up_dbb :: d -> DB -> DB
        up_dbb deltaBB db = up_bb newArr db
          where newArr = (bb db) .+ deltaBB

writeDelta :: DVal b db -> db -> Write
writeDelta (DVal f r df) = df

--data DVal' b db = Delta b db => DVal' (DB -> b) (b -> DB -> DB) (db -> DB -> da)
--liftBV :: (a -> b) -> (b -> a -> a) -> Val a -> Val b

liftBDV :: (Delta a da, Delta b db) => (a -> b) -> (b -> a -> a) -> (da -> db) -> (db -> a -> da) -> DVal a da -> DVal b db
liftBDV f b df db (DVal afor arev adrev) = DVal bfor brev bdrev
  where --bfor :: DB -> b
        bfor w = f (afor w)
        --brev :: b -> DB -> DB
        brev x w = arev (b x (afor w)) w
        --bdrev :: db -> DB -> DB
        bdrev dx w = arev ((afor w) .+ (db dx (afor w))) w

--data DBDelta = DBDeltaB (ListDelta Int) | DBDeltaBB (ListDelta Int)
data NNDelta = NNDeltaNNN (ListDelta Int)
instance Delta NN NNDelta where
  x .+ (NNDeltaNNN nu) = x { nnn = (nnn x .+ nu) }
  (.-) = undefined

--data DVal b db = Delta b db => DVal (DB -> b) (b -> DB -> DB) (db -> DB -> DB)
nnDval :: DVal NN NNDelta
nnDval = DVal nn up_nn drev
  where drev dx w = up_nn (nn w .+ dx) w

_dnnn :: DVal NN NNDelta -> DVal [Int] (ListDelta Int)
_dnnn = liftBDV nnn up_nnn undefined nrev
  where nrev :: (ListDelta Int) -> NN -> NNDelta
        nrev ldi nnnn = NNDeltaNNN ldi

nnnDval :: DVal [Int] (ListDelta Int)
nnnDval = _dnnn nnDval

dvread :: Delta a da => DVal a da -> DB -> a
dvread (DVal for _ _) = for

floo :: TMI ()
floo = do
  nnnDval <--. (Insert 1 52)
  return $ vconst ()

__deltaTmiDemo = do
  msp $ dvread nnDval thedb
  msp $ writeDelta nnDval (NNDeltaNNN (Insert 1 50)) thedb
  msp $ dvread nnnDval thedb
  msp $ writeDelta nnnDval (Insert 1 51) thedb
  ((), newDB) <- tmiRun thedb floo
  msp newDB

_deltaTmiDemo = do
  msp $ writeDelta _deltaB (Insert 1 200) thedb
  msp $ writeDelta _deltaBB (Insert 1 2000) thedb
  ((), newDB) <- tmiRun thedb froo
  msp newDB
  vsp _b
  msp $ ([1, 2, 3] :: [Int]) .+ [Insert 1 20 :: ListDelta Int, Insert 1 200 :: ListDelta Int]

-- ??
{-
(<--) :: Val a -> Val a -> TMI ()
dest <-- src = do
  writes <- get
  put $ writes ++ [mkwrite dest src]
  return $ vconst ()
-}
(<--.) :: Delta a da => DVal a da -> da -> TMI()
dest <--. srcDelta = do
  writes <- get
  put $ writes ++ [mkdwrite dest srcDelta]
  return $ vconst ()
--tmiRun :: DB -> TMI a -> IO (a, DB)

froo :: TMI ()
froo = do
  _deltaB <--. (Insert 1 200)
  _deltaBB <--. (Insert 1 2000)
  --_deltaBB <--. [Insert 1 2000, Insert 1 2001]
  return $ vconst ()

mkdwrite :: Delta a da => DVal a da -> da -> Write
mkdwrite = writeDelta

-- type Write = DB -> DB
-- mkwrite :: Val a -> Val a -> Write
-- data DVal a b da db = DVal (a -> b) (b -> a -> a) (da -> db) (db -> da)
-- -- Incremental variant of _b
-- _deltaB = DVal b up_b

processLines:: String -> (String -> IO ()) -> IO ()
processLines filename action = do
  bankCommand <- openFile filename ReadMode
  let loop = do
        eof <- hIsEOF bankCommand
        if eof
          then return ()
          else do
                  line <- hGetLine bankCommand
                  action line
                  loop
  loop

processBankCommandString :: String -> IO ()
processBankCommandString line = persistentRun $ processBankCommand (words line)

processBankCommand :: [String] -> TMI ()
processBankCommand ["createAccount", name] = do
  (_m name) _accounts <-- vconst 0
processBankCommand ["deposit", name, amount] = do
  let newBalance = (_m name) _accounts + vconst (read amount :: Int)
  (_m name) _accounts <-- newBalance
processBankCommand ["transfer", from, to, amount] = do
  (_m to) _accounts <-- (_m to) _accounts + vconst (read amount :: Int)
  (_m from) _accounts <-- (_m from) _accounts - vconst (read amount :: Int)
processBankCommand ["withdraw", from, amount] = do
  (_m from) _accounts <-- (_m from) _accounts - vconst (read amount :: Int)

bank args = do
  processBankCommand (map T.unpack args)
  return $ vconst $ WRRedirect "?q=%5B%22home%22%5D"

bankProcess = do
  --copyFile "init-history.db" "history.db"
  processLines "bank-commands.txt" processBankCommandString

bankPage :: [Text] -> WebTMI
bankPage [] = do
  let accountNames = (_keys _accounts)
  liftIO $ msp "ho"
  liftIO $ vsp accountNames
  liftIO $ vsp _accounts
  liftIO $ vsp theroot
  liftIO $ msp "ho2"
  return $ vconst $ WROk $ col [
    link "create foo" ["bank", "createAccount", "foo"],
    link "create bar" ["bank", "createAccount", "bar"],
    link "depost 100 foo" ["bank", "deposit", "foo", "100"],
    link "transfer 50" ["bank", "transfer", "foo", "bar", "50"],
    link "home" ["home"]
    ]

-- name contents attributes
data HTML = HTMLString Text | HTMLPair HTML HTML | HTMLNothing
  deriving Show
htmlRender :: HTML -> Text
htmlRender (HTMLString s) = s
htmlRender (HTMLPair a b) = (htmlRender a) `T.append` (htmlRender b)
htmlRender HTMLNothing = ""

tag :: Text -> Text -> [(Text, Text)] -> HTML
tag name contents attrs = HTMLString $ "<" <> name <> " " <> attrsS <> ">" <> contents <> "</" <> name <> ">"
  where attrsS = T.intercalate " " kevs
        kevs = [key <> "=" <> quot value | (key, value) <- attrs]
        quot s = "\"" <> s <> "\""
utag name = HTMLString $ "<" <> name <> "/>"

htmlList :: [HTML] -> HTML
htmlList htmls = mconcat htmls
col :: [HTML] -> HTML
col htmls = htmlList $ intersperse br htmls

br = utag "br"

link :: Text -> [Text] -> HTML
link text target = tag "a" text [("href", linkEncode target)]

linkEncode :: [Text] -> Text
linkEncode ss = "?q=" <> (T.pack $ ENC.encode $ show $ map T.unpack ss)

linkDecode :: Text -> [Text]
linkDecode s = read (ENC.decode $ T.unpack s)

instance Semigroup HTML where
  a <> b = HTMLPair a b

instance Monoid HTML where
  mempty = HTMLNothing

data WebResult = WROk HTML | WRRedirect String
  deriving Show

type WebTMI = TMI WebResult

instance ToResponse WebResult where
  toResponse (WROk html) = toResponse ((htmlRender $ html) :: Text, ok200, M.fromList [("Content-type", ["text/html"])] :: HeaderMap)
  toResponse (WRRedirect url) = toResponse ("" :: Text, found302, M.fromList [("Location", [T.pack url])] :: HeaderMap)

registry :: M.Map Text ([Text] -> WebTMI)
registry = M.fromList
  [ ("home", bankPage)
  , ("bank", bank)
  ]

oldWebDemo :: IO ()
oldWebDemo = run 3001 $ do
  route "/" $ do
    q <- fromMaybe defaultRoute <$> getQuery "q"
    liftIO $ msp $ linkDecode q
    let action = linkDecode q
        command:args = action
        webTmi = (registry M.! command) args
    webResult <- liftIO $ persistentRun webTmi
    liftIO $ msp webResult
    return $ webResult
  where defaultRoute = "%5B%22home%22%5D"
