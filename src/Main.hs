--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.State hiding (lift)
import Data.Containers.ListUtils
--import Data.Dynamic
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
--import GHC.Generics hiding (V1)

import Hash
import Tmi
import Util

-- This is the only way to make a V without applying an F to an existing one
makeRoot :: V W
makeRoot =
  -- Recursive use of this v
  let v = V { getKey = worldKey
            , getArgKeys = []
            , runForward = undefined
            , runReverse = undefined }
   in v
worldKey :: Key
worldKey = Key (hash "")  -- md5 hash of empty string will probably not collide with any V

_anInt :: V W -> V Int
_anInt = lift $ F "anInt" anInt anInt_r
  where anInt_r w i = w { anInt = i }

_aString :: V W -> V String
_aString = lift $ F "aString" aString aString_r
  where aString_r w s = w { aString = s }

incer :: F Int Int
incer = F "incer" (+1) (\_ x -> x-1)

nextInt = lift incer

bother :: V Int -> V String -> V (Int, String)
bother = lift2 $ F2 "bother" (,) (\_ _ (i, s) -> (i, s))  -- Yeah I wanted to write it out

main = do
  noBuffering
  let world :: W
      world = W { anInt = 12, aString = "asdf" }
      --h = initHistory world
      vw = makeRoot
      vai = _anInt vw
      vas = _aString vw
      vni = nextInt vai
      vboth = bother vni vas
      write = Write vni 130
      --h' = updateHistory h [write]
  -- let writes = propagateWrites (case h of History vcs -> last vcs) [write]
  -- msp writes
  -- let hmm :: M.Map Wrapped Int
  --     hmm = M.insert (wrapV vai) 88 (M.insert (wrapV vboth) 99 M.empty)
  msp "hi"
