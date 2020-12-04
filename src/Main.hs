{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.State hiding (lift)

import History
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

idF :: (Nice a, Integral a) => V a -> V a
idF = hoist_1_1 $ F {..}
  where ffor = id
        frev _ x = x
        name = "idF"

anotherV = idF (incF aV)

splitF :: (Nice a, Integral a) => V a -> (V a, V a)
splitF = hoist_1_2 $ F_1_2 {..}
  where ffor_1_2 x = (x', x'')
          where x' = x `div` 2
                x'' = x - x'
        frev_1_2 _ (x, x') = x + x'
        name_1_2 = "splitF"

(leftV, rightV) = splitF anotherV

andPlusV = plusF leftV rightV

main = do
  -- msp ("leftV", leftV)
  -- msp ("rightV", rightV)
  tmiRun @W @Dum world $ do
    listen aW $ \i -> do
      msp ("aW", i)
    listen aV $ \i -> do
      msp ("aV", i)
    listen anotherV $ \i -> do
      msp ("anotherV", i)
    listen leftV $ \i -> do
      msp ("leftV", i)
    listen rightV $ \i -> do
      msp ("rightV", i)
    listen andPlusV $ \x -> do
      msp ("andPlusV", x)
    -- dump

    -- start
    --                                                         50
    -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
    -- w            100                 101                 \ rightV /          101
    --                                                         51

    leftV <--. 200
    --                                                         200/125
    -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
    -- w            250                 251                 \ rightV /          251
    --                                                         51/126

    rightV <--. 201
    --                                                         125
    -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
    -- w            325                 326                 \ rightV /          326
    --                                                         201

    andPlusV <--. 203
    --                                                         101
    -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
    -- w            202                 203                 \ rightV /          203
    --                                                         102

  msp "hi"
