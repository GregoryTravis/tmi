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
import Data.Dynamic

import Tmi
import Util

world :: W
world = W { anInt = 10, anotherInt = 20 }

history :: SimpleHistory W
history = mkHistory world

inc_hy :: R Int -> R Int
inc_hy (R x rx) = R x' rx'
  where x' = x + 1
        rx' = Receiver "inc_hy" $ \x ->
          rx <-- (x - 1)
inc_for :: Int -> Int
inc_for = (+1)
inc_rev :: R Int -> Int -> Write
inc_rev (R x rx) newX =
  rx <-- newX - 1
inc_hy' :: R Int -> R Int
inc_hy' = hybrid1 inc_for inc_rev

plus_hy :: R Int -> R Int -> R Int
plus_hy (R x rx) (R y ry) = R z rz
  where z = x + y
        rz = Receiver "plus_hy" $ \newZ ->
          let x' = newZ `div` 2
              y' = newZ - x'
           in (rx <-- x') <>
              (ry <-- y')
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

_anInt :: V (R W -> R Int)
_anInt = VConst __anInt
  where __anInt (R w rw) = (R i ri)
          where i = anInt w
                ri = Receiver "_anInt" $ \newI ->
                    rw <-- w { anInt = newI }

_anotherInt :: V (R W -> R Int)
_anotherInt = VConst __anotherInt
  where __anotherInt (R w rw) = (R i ri)
          where i = anotherInt w
                ri = Receiver "_anotherInt" $ \newI ->
                    rw <-- w { anotherInt = newI }

-- TODO maybe tf for this?
incV :: V (R Int -> R Int)
incV = VConst inc_hy

plus :: V (R Int -> R Int -> R Int)
plus = VConst plus_hy

split :: V (R Int -> R (Int, Int))
split = VConst $ hybrid1 for rev
  where for x = (x', x'')
          where x' = x `div` 2
                x'' = x - x'
        rev (R _ rx) (x', x'') =
          rx <-- (x' + x'')

splitted :: V (Int, Int)
splitted = split <$$> inced

idV :: Typeable a => V (R a -> R a)
idV = VConst $ hybrid1 for rev
  where for x = x
        rev (R _ rx) x = rx <-- x

-- root = VRoot

vw :: V W
vw = getRoot history

anIntV = _anInt <$$> vw
anotherIntV = _anotherInt <$$> vw
inced = incV <$$> anIntV
plusPartialV = plus <**> anIntV
plusPartialV' = plusPartialV <**> anotherIntV
sumV = plusPartialV <$$> anotherIntV
sumV' = VSeal plusPartialV'
sumV'' = plus <**> anIntV <$$> anotherIntV

main = do
  -- msp $ r world vw
  -- msp $ r world anIntV
  -- msp $ r world inced
  -- msp $ r world sumV
  -- msp $ r world sumV'
  -- msp $ wr world anIntV 100
  -- msp $ wr world anIntV 100
  -- msp $ wr world sumV 100
  -- msp $ wr world sumV' 100
  -- msp $ wr world sumV'' 100
  -- msp $ wr world (VApp incV sumV) 201
  -- msp $ wr world (VApp incV sumV') 201
  -- msp $ wr world (VApp incV sumV'') 201
  msp $ r history splitted
  msp $ r history (idV <$$> splitted)
  msp $ wr history splitted (8, 9)
  msp $ wr history (idV <$$> splitted) (8, 9)
  msp "curry hi"

-- $> :module +*Curry
--
-- $> :t sumV''
--
-- $> :t inced
--
-- $> :t splitted
--
-- $> main

-- _main = do
--   -- msp ("leftV", leftV)
--   -- msp ("rightV", rightV)
--   tmiRun @W @Dum world $ do
--     listen aW $ \i -> do
--       msp ("aW", i)
--     listen aV $ \i -> do
--       msp ("aV", i)
--     listen anotherV $ \i -> do
--       msp ("anotherV", i)
--     listen leftV $ \i -> do
--       msp ("leftV", i)
--     listen rightV $ \i -> do
--       msp ("rightV", i)
--     listen andPlusV $ \x -> do
--       msp ("andPlusV", x)
--     -- dump

--     -- start
--     --                                                         50
--     -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
--     -- w            100                 101                 \ rightV /          101
--     --                                                         51

--     leftV <--. 200
--     --                                                         200/125
--     -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
--     -- w            250                 251                 \ rightV /          251
--     --                                                         51/126

--     rightV <--. 201
--     --                                                         125
--     -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
--     -- w            325                 326                 \ rightV /          326
--     --                                                         201

--     andPlusV <--. 203
--     --                                                         101
--     -- aW -anIntF-> aV -incF-> * -idF-> anotherV -splitF->+- leftV  -+plusV-> andPLusV
--     -- w            202                 203                 \ rightV /          203
--     --                                                         102

--   msp "hi"
