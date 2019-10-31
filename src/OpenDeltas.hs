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

-- The following is the result of a lengthy effort with many failed attempts
-- along the way that I just cleaned out in the last batch of changes. The goal
-- was open deltas: allowing the programmer to create as many deltas they want
-- for any given type, and implementations of the reverse delta transform for
-- each one. Now that I think about it, I didn't actually test whether it's
-- possible to have more than one. Sounds crazy, but it's true. What I did
-- manage to do was to create a class, Inc, which is parameterized by input and
-- output types a, b; delta types da, db; and another type that is an instance
-- of a singleton class that uniquely determines a particular function. So the
-- idea is that a 

module OpenDeltas (deltaTmiDemo) where

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

data Blerb = Blerb { wba :: [Int] } deriving Show
data World = World { wa :: [Int], wb :: Blerb } deriving Show
worldData :: World
worldData = World { wa = [1, 2, 3], wb = Blerb { wba = [4, 5, 6] } }
u_wa wa w = w { wa = wa }
u_wb wb w = w { wb = wb }
u_wba wba w = w { wba = wba }

class Delta a d | d -> a where
  (.+) :: a -> d -> a
  (.-) :: a -> a -> d

data ListDelta a = Insert Int a | Delete Int
deriving instance Show a => Show (ListDelta a)

instance Delta [a] (ListDelta a) where
  xs .+ (Insert i x) = (take i xs) ++ [x] ++ (drop i xs)
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

-- Function abstraction
data Brap a b = Brap (a -> b) (b -> a -> a)
bmap :: Brap a b -> BMap (Brap a b) (Brap [a] [b])
bmap (Brap f r) = BMap (Brap f r) (Brap (map f) rev)
  where rev bs as = map (\(b, a) -> r b a) (zip bs as)

data BMap f br = BMap f br

class Guff a b c | c -> a b where
  getIt :: c -> Brap a b
instance Guff [a] [b] (BMap (Brap a b) (Brap [a] [b])) where
  getIt (BMap f mf) = mf

-- Wrapped as singleton
gbincr :: BMap (Brap Int Int) (Brap [Int] [Int])
--gbincr = BMap bincr (bmap bincr)
gbincr = bmap bincr
gbdubs = bmap bdubs

class (Delta a da, Delta b db, Guff a b g) => Inc a b da db g | g db -> da where
  appInc :: g -> db -> a -> da

--instance (Delta a da, Delta b db) => Inc a b da db (BMap (Brap a b)) where
instance Inc [a] [b] (ListDelta a) (ListDelta b) (BMap (Brap a b) (Brap [a] [b])) where
  -- TODO: could choose to not lookup in as since it's not used; or a different form for this
  appInc (BMap (Brap f r) _) (Insert i b) as = Insert i (r b a)
    where a = as !! i

data CompInc bc ab = CompInc bc ab
instance (Guff b c bc, Guff a b ab) => Guff a c (CompInc bc ab) where
  getIt = undefined
  -- -- TODO wish I didn't need to write this since I'm never going to call it?
  -- getIt (CompInc bc ab) = Brap acf acr
  --   where (Brap bcf bcr) = getIt bc
  --         (Brap abf abr) = getIt ab
  --         acf =

instance (Inc b c db dc gbc, Inc a b da db gab) => Inc a c da dc (CompInc gbc gab) where
  --appInc = undefined
  appInc (CompInc gbc gab) dc a = da
    where db = appInc gbc dc b
          (Brap abf abr) = getIt gab
          b  = abf a  -- TODO an bapplyf for this
          da = appInc gab db a
    -- where (Brap bcf bcr) = getIt gbc
    --       (Brap abf abr) = getIt gab

bapplyf (Brap f r) = f
bapplyr (Brap f r) = r
bincr = Brap (+1) (\n _ -> n - 1)
bdubs = Brap (*2) (\n _ -> n `div` (2::Int))

gbdi :: BMap (Brap Double Integer) (Brap [Double] [Integer])
gbdi = bmap $ Brap floor (\i _ -> (fromInteger i) :: Double)
gbis :: BMap (Brap Integer String) (Brap [Integer] [String])
gbis = bmap $ Brap show (\s _ -> read s)

-- TODO wish I could use DatatypeContexts but I know it's wrong and you should use GADTs or somethign
--data Guff DB b c => GVal b c = GVal (DB -> b) (b -> DB -> DB) c
data GVal b g = GVal g

thegdb :: GVal World (BId (Brap World World))
thegdb  = GVal gbid

gbid :: BId (Brap a a)
gbid = BId (Brap id (\x _ -> x))
data BId br = BId br  -- TODO why is this 'br'?
instance Guff a b (BId (Brap a b)) where
  getIt (BId mf) = mf
werld :: GVal World (BId (Brap World World))
werld = GVal gbid
--bmap :: Brap a b -> BMap (Brap a b) (Brap [a] [b])
--bmap (Brap f r) = BMap (Brap f r) (Brap (map f) rev)

gread :: Guff World b g => GVal b g -> World -> b
gread (GVal g) w = f w
  where (Brap f r) = getIt g

data BWa br = BWa br
instance Guff a b (BWa (Brap a b)) where
  getIt (BWa mf) = mf
gbwa :: BWa (Brap World [Int])
gbwa = BWa (Brap wa u_wa)
gvwa :: GVal [Int] (BWa (Brap World [Int]))
gvwa = GVal gbwa

data BWb br = BWb br
instance Guff a b (BWb (Brap a b)) where
  getIt (BWb mf) = mf
gbwb :: BWb (Brap World Blerb)
gbwb = BWb (Brap wb u_wb)
gvwb :: GVal Blerb (BWb (Brap World Blerb))
gvwb = GVal gbwb

data BWba br = BWba br
instance Guff a b (BWba (Brap a b)) where
  getIt (BWba mf) = mf
gbwba :: BWba (Brap Blerb [Int])
gbwba = BWba (Brap wba u_wba)
gvwba :: GVal Blerb (BWba (Brap Blerb [Int]))
gvwba = GVal gbwba

-- a -> b1 -> b2 or ((b1 -> b2) b1)
-- db2 -> a -> da is the composite delta transformer
--fah :: (Guff a b f, Guff w a v) => f -> v -> q
--fah fg (GVal vg) = (appInc (CompInc fg vg), CompInc fg vg)
fah :: funG -> (GVal a valG) -> GVal b (CompInc funG valG)
fah fg (GVal vg) = GVal (CompInc fg vg)
  -- where (Brap ff fr) = getIt fg
  --       (Brap vf vr) = getInt vg

data WADelta = WADelta (ListDelta Int) deriving Show
instance Delta World WADelta where
  w .+ (WADelta ld) = w { wa = (wa w .+ ld) }
  (.-) = undefined

data BlerbDelta = BlerbDelta (ListDelta Int) deriving Show
instance Delta Blerb BlerbDelta where
  b .+ (BlerbDelta ld) = b { wba = (wba b) .+ ld }
  (.-) = undefined

data WBDelta = WBDelta BlerbDelta deriving Show
instance Delta World WBDelta where
  w .+ (WBDelta bd) = w { wb = (wb w .+ bd) }
  (.-) = undefined

-- instance Delta World () where
--   w .+ () = w
--   (.-) = undefined

instance Inc World [Int] WADelta (ListDelta Int) (BWa (Brap World [Int])) where
  appInc g ld _ = WADelta ld

--instance Inc World World () WADelta (GVal World (BId (Brap World World))) where
-- instance Inc World World () WADelta (BId (Brap World World)) where
--   appInc = undefined

instance Inc World World WADelta WADelta (BId (Brap World World)) where
  appInc g waDelta _ = waDelta

instance Inc World World WBDelta WBDelta (BId (Brap World World)) where
  appInc g wbDelta _ = wbDelta

instance Inc Blerb [Int] BlerbDelta (ListDelta Int) (BWba (Brap Blerb [Int])) where
  appInc g ld _ = BlerbDelta ld

instance Inc World Blerb WBDelta BlerbDelta (BWb (Brap World Blerb)) where
  appInc g bd _ = WBDelta bd

xxxx = fah gbwa werld
--yyyy
  -- :: (Inc a World da db (GVal World (BId (Brap World World))),
  --     Inc World [Int] db (ListDelta Int) (BWa (Brap World [Int]))) =>
  --    a -> da

  -- :: Inc a World da WADelta (GVal World (BId (Brap World World))) =>
  --    a -> da
--yyyy = undefined
--yyyy = (appInc (CompInc gbwa werld) (Insert 1 (12::Int))) :: ListDelta Int -> WADelta
--yyyy = appInc (CompInc gbwa werld) (Insert 1 (12::Int))

{-
-- ** I think what this needs is a Guff instance just for application
gapply :: (Guff a b funG, Guff World a valG, Guff World b resultG) =>
          funG -> GVal a valG -> GVal b resultG
--gapply = undefined
gapply funG (GVal valG) = GVal resultG
  where resultG = CompInc funG valG
{-
  where (Brap funF funR) = getIt funG
        (Brap argF argR) = getIt argG
        resultG = Brap resultF resultR
        resultF :: World -> b
        resultF w = funF (argF w)
        resultR :: b -> World -> World
        resultR b w = argR (funR b a) w
          where a = argF w
-}
-}

gapply funG (GVal valG) = GVal resultG
  where resultG = CompInc funG valG

dwrot (GVal g) delta w = appInc g delta w

deltaTmiDemo = do
  msp $ gread werld worldData
  msp $ gread gvwa worldData
  msp $ appInc gbid (WADelta (Insert 1 (12::Int))) worldData
  msp $ appInc (CompInc gbwa gbid) (Insert 1 (12::Int)) worldData
  --msp $ appInc (gapply gbwa werld) (Insert 1 (12::Int)) worldData
  msp $ appInc (case gapply gbwa werld of (GVal ha) -> ha) (Insert 1 (12::Int)) worldData
  msp $ dwrot (gapply gbwa werld) (Insert 1 (12::Int)) worldData
  msp $ gread gvwb worldData
  msp $ appInc (CompInc gbwba gbwb) (Insert 1 (34::Int)) worldData
  msp $ appInc (case gapply gbwb werld of (GVal ha) -> ha) (BlerbDelta (Insert 1 (12::Int))) worldData
  msp $ appInc (case gapply gbwba (gapply gbwb werld) of (GVal ha) -> ha) (Insert 1 (12::Int)) worldData
  msp $ dwrot (gapply gbwb werld) (BlerbDelta (Insert 1 (12::Int))) worldData
  msp $ dwrot (gapply gbwba (gapply gbwb werld)) (Insert 1 (12::Int)) worldData
  let j = dwrot (gapply gbwb werld) (BlerbDelta (Insert 1 (12::Int))) worldData
      jj = dwrot (gapply gbwba (gapply gbwb werld)) (Insert 1 (12::Int)) worldData
  msp $ worldData .+ j
  msp $ worldData .+ jj
  --msp $ dwrot (gapply gbwb werld) (BlerbDelta (Insert 1 (12::Int))) worldData
  -- -- works
  -- msp $ ((appInc gbincr (Insert 1 (21::Int)) [11::Int, 31, 41]) :: (ListDelta Int))
  -- msp $ ((appInc gbdubs (Insert 1 (10::Int)) [5::Int, 15, 20]) :: (ListDelta Int))
  -- let dubThenInc = CompInc gbincr gbdubs
  -- msp $ ((appInc dubThenInc (Insert 1 (21::Int)) [5::Int, 15, 20]) :: (ListDelta Int))
  -- -- d -> i -> s
  -- msp $ ((appInc gbdi (Insert 1 (20::Integer)) [1.0::Double, 2.0, 3.0]) :: ListDelta Double)
  -- msp $ ((appInc gbis (Insert 1 ("20"::String)) [1::Integer, 2, 3]) :: ListDelta Integer)
  -- let gbdis = CompInc gbis gbdi
  -- msp $ ((appInc gbdis (Insert 1 ("20"::String)) [1.0::Double, 2.0, 3.0]) :: ListDelta Double)

  -- msp $ bapplyf bincr 3
  -- msp $ bapplyr bincr 4 3
  -- msp $ bapplyf bdubs 5
  -- msp $ bapplyr bdubs 10 5
  -- msp $ bapplyf (bmap bincr) [1, 2 ,3]
  -- msp $ bapplyr (bmap bincr) [2, 3, 4] [1, 2 ,3]
  -- --msp $ bapplyr (bmap bincr) [2, 3, 4] undefined  -- wish this worked
  msp "hi"

