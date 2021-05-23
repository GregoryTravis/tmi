module Main where

import Control.Monad.State hiding (lift)
import Data.Dynamic

import Ext
import Tmi
import Util

data W = W { anInt :: Int, anotherInt :: Int }
  deriving (Read, Show)

world :: W
world = W { anInt = 10, anotherInt = 20 }

history :: History W
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

listeny :: Show a => a -> IO ()
listeny x = putStrLn $ "Listeny: " ++ (show x)

action :: StateT (TmiState W) IO ()
action = do
  listen splitted listeny
  listen anIntV listeny
  splitted <--- VConst (80, 90)

main = extMain
_main = do
  -- writeHistory "history.db" history
  (a, history') <- tmiRun history action
  msp a
  msp history'
  () <- persistentTmiRun "history.db" action
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
