{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyno
( Dyno
, getit
, getTypeRep
--, getit'
, mkDyno ) where

import Data.Typeable

-- Method taken from
--   https://stackoverflow.com/a/10422679/5265393
--   https://stackoverflow.com/users/81636/gatoatigrado
--   https://pastebin.com/KiJqqmpj

-- TODO grab the typerep so we can print out two types if they are different
-- types, rather than just saying they're not equal. Not in Eq but a separate
-- thing.
data Dyno = Dyno { getit :: forall a. (Eq a, Typeable a) => Maybe a
                 , getTypeRep :: TypeRep
                 , compareTo :: Dyno -> Bool }

-- In case you ever want to make this Ord, you can also compare TypeReps
mkDyno :: forall a. (Eq a, Typeable a) => a -> Dyno
mkDyno x = Dyno { getit = cast x
                , getTypeRep = typeOf x
                , compareTo }
  where compareTo (Dyno { getit = getit' }) =
          let x' = (getit' :: Maybe a)
           in case x' of Just x' -> x == x'
                         Nothing -> False

instance Eq Dyno where
  a == b = compareTo a b

instance Show Dyno where
  show (Dyno {..}) = "<<Dyno " ++ show getTypeRep ++ ">>"

getit' :: (Eq a, Typeable a) => Dyno -> a
getit' dyno = case getit dyno of Just x -> x
                                 Nothing -> error "getit'"

-- data Yamp = Bleh Int | Vort String deriving Eq

-- msp :: Show a => a -> IO ()
-- msp = putStrLn . show

-- test = do
--   let d0 = mkDyno (3::Int)
--       d1 = mkDyno (3::Int)
--       d2 = mkDyno (4::Int)
--       d3 = mkDyno (4::Float)
--       d4 = mkDyno (Bleh 34)
--   msp "start"
--   msp $ compareTo d0 d0
--   msp $ compareTo d0 d1
--   msp $ compareTo d0 d2
--   msp $ compareTo d0 d3
--   msp $ compareTo d0 d4
--   msp $ d0 == d0
--   msp $ d0 == d1
--   msp $ d0 == d2
--   msp $ d0 == d3
--   msp $ d0 == d4
--   let i0 :: Int
--       i0 = case getit d0 of Just x -> x
--   let i1 :: Int
--       i1 = case getit' d4 of Just x -> x
--   msp i0
--   msp i1
--   msp "hitest"
