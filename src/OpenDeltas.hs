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
-- each one. I think I crudely re-invented Haskell's weird version of dependent
-- types.
--
-- Fun a b: a bidi function from a to b.
--
-- Label: a singleton typeclass used to identify a function at the type level.
-- For example, BMap is the label for the bidi map, bmap.
--
-- Val b g: a bidirection value of type b. g is an instance of class Label.
--
-- Inc: a class used to define a delta transform implementation, which
-- transforms an output delta to an input delta.
--
-- Fun is a lensy thing, but I didn't actually implement it fully here.  All this demo does is show that:
--   - you can transform the deltas
--   - you can have multiple delta types for a class, and thus multiple implementations
--   - you can write a delta to a value and get back the updated World
--
-- In particular, I don't think I implemented the non-incremental part of the
-- lens.  gapply is composition of the reverse delta part of the lens, but not
-- the non-incremental part.
--
-- The type witchery in here is pretty verbose and most likely could be
-- simpler, but this was so hard to get working that I'm done with it for now.
-- Once I get some kind of TH or syntax extension so that I can generate all
-- the boilerplate. You'll also notice that I have explicit type declarations
-- everywhere. Possibly I can remove some, but probably not enough.
--
-- Notably, the output type of Val is a phantom type. The only thing that a Val
-- contains is the Label for the function that produced it. I didn't think "gee
-- a phantom type would be good for this", I just didn't need a value of type
-- 'b' in there, so there isn't one.

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

data ConsDelta a = Cons a | Snoc a
deriving instance Show a => Show (ConsDelta a)

instance Delta [a] (ConsDelta a) where
  xs .+ (Cons x) = x : xs
  xs .+ (Snoc x) = xs ++ [x]
  (.-) = undefined  -- slow

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
data Fun a b = Fun (a -> b) (b -> a -> a)
bmap :: Fun a b -> BMap (Fun a b) (Fun [a] [b])
bmap (Fun f r) = BMap (Fun f r) (Fun (map f) rev)
  where rev bs as = map (\(b, a) -> r b a) (zip bs as)

data BMap f br = BMap f br

class Label a b c | c -> a b where
  getIt :: c -> Fun a b
instance Label [a] [b] (BMap (Fun a b) (Fun [a] [b])) where
  getIt (BMap f mf) = mf

-- Wrapped as singleton
gbincr :: BMap (Fun Int Int) (Fun [Int] [Int])
--gbincr = BMap bincr (bmap bincr)
gbincr = bmap bincr
gbdubs = bmap bdubs

class (Delta a da, Delta b db, Label a b g) => Inc a b da db g | g db -> da where
  appInc :: g -> db -> a -> da

--instance (Delta a da, Delta b db) => Inc a b da db (BMap (Fun a b)) where
instance Inc [a] [b] (ListDelta a) (ListDelta b) (BMap (Fun a b) (Fun [a] [b])) where
  -- TODO: could choose to not lookup in as since it's not used; or a different form for this
  appInc (BMap (Fun f r) _) (Insert i b) as = Insert i (r b a)
    where a = as !! i

instance Inc [a] [b] (ConsDelta a) (ConsDelta b) (BMap (Fun a b) (Fun [a] [b])) where
  -- TODO: could choose to not lookup in as since it's not used; or a different form for this
  appInc (BMap (Fun f r) _) (Cons x) _ = Cons (r x undefined)
  appInc (BMap (Fun f r) _) (Snoc x) _ = Snoc (r x undefined)

data CompInc bc ab = CompInc bc ab
instance (Label b c bc, Label a b ab) => Label a c (CompInc bc ab) where
  getIt = undefined
  -- -- TODO wish I didn't need to write this since I'm never going to call it?
  -- getIt (CompInc bc ab) = Fun acf acr
  --   where (Fun bcf bcr) = getIt bc
  --         (Fun abf abr) = getIt ab
  --         acf =

instance (Inc b c db dc gbc, Inc a b da db gab) => Inc a c da dc (CompInc gbc gab) where
  --appInc = undefined
  appInc (CompInc gbc gab) dc a = da
    where db = appInc gbc dc b
          (Fun abf abr) = getIt gab
          b  = abf a  -- TODO an bapplyf for this
          da = appInc gab db a
    -- where (Fun bcf bcr) = getIt gbc
    --       (Fun abf abr) = getIt gab

bapplyf (Fun f r) = f
bapplyr (Fun f r) = r
bincr = Fun (+1) (\n _ -> n - 1)
bdubs = Fun (*2) (\n _ -> n `div` (2::Int))

gbdi :: BMap (Fun Double Integer) (Fun [Double] [Integer])
gbdi = bmap $ Fun floor (\i _ -> (fromInteger i) :: Double)
gbis :: BMap (Fun Integer String) (Fun [Integer] [String])
gbis = bmap $ Fun show (\s _ -> read s)

-- TODO wish I could use DatatypeContexts but I know it's wrong and you should use GADTs or somethign
--data Label DB b c => Val b c = Val (DB -> b) (b -> DB -> DB) c
data Val b g = Val g

thegdb :: Val World (BId (Fun World World))
thegdb  = Val gbid

gbid :: BId (Fun a a)
gbid = BId (Fun id (\x _ -> x))
data BId br = BId br  -- TODO why is this 'br'?
instance Label a b (BId (Fun a b)) where
  getIt (BId mf) = mf
werld :: Val World (BId (Fun World World))
werld = Val gbid
--bmap :: Fun a b -> BMap (Fun a b) (Fun [a] [b])
--bmap (Fun f r) = BMap (Fun f r) (Fun (map f) rev)

gread :: Label World b g => Val b g -> World -> b
gread (Val g) w = f w
  where (Fun f r) = getIt g

data BWa br = BWa br
instance Label a b (BWa (Fun a b)) where
  getIt (BWa mf) = mf
gbwa :: BWa (Fun World [Int])
gbwa = BWa (Fun wa u_wa)
gvwa :: Val [Int] (BWa (Fun World [Int]))
gvwa = Val gbwa

data BWb br = BWb br
instance Label a b (BWb (Fun a b)) where
  getIt (BWb mf) = mf
gbwb :: BWb (Fun World Blerb)
gbwb = BWb (Fun wb u_wb)
gvwb :: Val Blerb (BWb (Fun World Blerb))
gvwb = Val gbwb

data BWba br = BWba br
instance Label a b (BWba (Fun a b)) where
  getIt (BWba mf) = mf
gbwba :: BWba (Fun Blerb [Int])
gbwba = BWba (Fun wba u_wba)
gvwba :: Val Blerb (BWba (Fun Blerb [Int]))
gvwba = Val gbwba

-- a -> b1 -> b2 or ((b1 -> b2) b1)
-- db2 -> a -> da is the composite delta transformer
--fah :: (Label a b f, Label w a v) => f -> v -> q
--fah fg (Val vg) = (appInc (CompInc fg vg), CompInc fg vg)
fah :: funG -> (Val a valG) -> Val b (CompInc funG valG)
fah fg (Val vg) = Val (CompInc fg vg)
  -- where (Fun ff fr) = getIt fg
  --       (Fun vf vr) = getInt vg

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

instance Inc World [Int] WADelta (ListDelta Int) (BWa (Fun World [Int])) where
  appInc g ld _ = WADelta ld

--instance Inc World World () WADelta (Val World (BId (Fun World World))) where
-- instance Inc World World () WADelta (BId (Fun World World)) where
--   appInc = undefined

instance Inc World World WADelta WADelta (BId (Fun World World)) where
  appInc g waDelta _ = waDelta

instance Inc World World WBDelta WBDelta (BId (Fun World World)) where
  appInc g wbDelta _ = wbDelta

instance Inc Blerb [Int] BlerbDelta (ListDelta Int) (BWba (Fun Blerb [Int])) where
  appInc g ld _ = BlerbDelta ld

instance Inc World Blerb WBDelta BlerbDelta (BWb (Fun World Blerb)) where
  appInc g bd _ = WBDelta bd

xxxx = fah gbwa werld
--yyyy
  -- :: (Inc a World da db (Val World (BId (Fun World World))),
  --     Inc World [Int] db (ListDelta Int) (BWa (Fun World [Int]))) =>
  --    a -> da

  -- :: Inc a World da WADelta (Val World (BId (Fun World World))) =>
  --    a -> da
--yyyy = undefined
--yyyy = (appInc (CompInc gbwa werld) (Insert 1 (12::Int))) :: ListDelta Int -> WADelta
--yyyy = appInc (CompInc gbwa werld) (Insert 1 (12::Int))

{-
-- ** I think what this needs is a Label instance just for application
gapply :: (Label a b funG, Label World a valG, Label World b resultG) =>
          funG -> Val a valG -> Val b resultG
--gapply = undefined
gapply funG (Val valG) = Val resultG
  where resultG = CompInc funG valG
{-
  where (Fun funF funR) = getIt funG
        (Fun argF argR) = getIt argG
        resultG = Fun resultF resultR
        resultF :: World -> b
        resultF w = funF (argF w)
        resultR :: b -> World -> World
        resultR b w = argR (funR b a) w
          where a = argF w
-}
-}

gapply funG (Val valG) = Val resultG
  where resultG = CompInc funG valG

dwrot (Val g) delta w = appInc g delta w

deltaTmiDemo = do
  msp $ gread werld worldData
  msp $ gread gvwa worldData
  msp $ appInc gbid (WADelta (Insert 1 (12::Int))) worldData
  msp $ appInc (CompInc gbwa gbid) (Insert 1 (12::Int)) worldData
  --msp $ appInc (gapply gbwa werld) (Insert 1 (12::Int)) worldData
  msp $ appInc (case gapply gbwa werld of (Val ha) -> ha) (Insert 1 (12::Int)) worldData
  msp $ dwrot (gapply gbwa werld) (Insert 1 (12::Int)) worldData
  msp $ gread gvwb worldData
  msp $ appInc (CompInc gbwba gbwb) (Insert 1 (34::Int)) worldData
  msp $ appInc (case gapply gbwb werld of (Val ha) -> ha) (BlerbDelta (Insert 1 (12::Int))) worldData
  msp $ appInc (case gapply gbwba (gapply gbwb werld) of (Val ha) -> ha) (Insert 1 (12::Int)) worldData
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
  let dubThenInc = CompInc gbincr gbdubs
  msp $ ((appInc dubThenInc (Insert 1 (21::Int)) [5::Int, 15, 20]) :: (ListDelta Int))
  msp $ ((appInc dubThenInc (Cons (21::Int)) [5::Int, 15, 20]) :: (ConsDelta Int))
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

