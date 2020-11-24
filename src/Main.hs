{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Evaluator
import Hash
import Tmi
import Util

data W = W { anInt :: Int }
  deriving (Eq, Show)

anIntF :: V W -> V Int
anIntF = hoist_1_1 $ F {..}
  where ffor = anInt
        frev w i = w { anInt = i }
        name = "anIntF"

world :: W
world = W { anInt = 100 }
aW = mkRoot world

-- 'plus' lens: adds forwards; backwards, splits value into two roughly equal halves
-- The 'F' suffix isn't right, these aren't Fs
plusF :: (Nice a, Integral a) => V a -> V a -> V a
plusF = hoist_2_1 $ F2 {..}
  where ffor2 = (+)
        frev2 _ _ x = (x `div` 2, x - (x `div` 2))
        name2 = "plusF"

-- TODO If I annotate this, I get weird typeclass errors
-- aV :: (Show a, Typeable a, Integral a) => V a
aV = anIntF aW

incF :: (Nice a, Integral a) => V a -> V a
incF = hoist_1_1 $ F {..}
  -- TODO: why can't these be (+1) and (subtract 1) -- it says it can't deduce Num (from Integral??)
  where ffor x = x + 1
        frev _ x = x - 1
        name = "incF"

anotherV = incF aV

splitF :: (Nice a, Integral a) => V a -> (V a, V a)
splitF = hoist_1_2 $ F_1_2 {..}
  where ffor_1_2 x = (x', x'')
          where x' = x `div` 2
                x'' = x - x'
        frev_1_2 _ (x, x') = x + x'
        name_1_2 = "splitF"

main = do
  msp ("aW", aW)
  msp ("aV", aV)
  --msp ("aW N", vN aW)
  let (leftV, rightV) = splitF anotherV
  let write'' = Write (dyv leftV) (dy (200::Int))
  let write' = Write (dyv rightV) (dy (101::Int))

  let h = mkHistory world :: Dum W
      h' = addListener (addListener h (mkListener leftV msp)) (mkListener rightV msp)
  h'' <- write h' [write'']
  let latest = case h'' of Dum ws _ -> head ws
  msp world
  msp latest

  -- Should fail, because of conflict -- yes it does
  -- let w0 = Write (dyv leftV) (dy (100::Int))
  --     w1 = Write (dyv leftV) (dy (200::Int))
  -- h''' <- write h' [w0, w1]

  msp "hi"
