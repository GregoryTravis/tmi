--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Dynamic
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
--import GHC.Generics hiding (V1)

import Util

data Key = Key Int deriving (Eq, Ord, Show)

class Hashable a => Keyable a where
  toKey :: a -> Key
  toKey = Key . hash

instance Keyable Int

data Named a = Named String a
unName :: Named a -> a
unName (Named _ x) = x
nameOf :: Named a -> String
nameOf (Named s _) = s

instance Hashable (Named a) where
  hash = hash . nameOf
  hashWithSalt = hws

data V a = V0 { val :: a  }
         | forall b. (Hashable b, Typeable b) => V1 { for :: Named (b -> a)
                                                    , rev :: Named (b -> a -> b)
                                                    , arg1_0 :: V b }
         | forall b c. (Hashable b, Typeable b, Hashable c, Typeable c) => V2 { for2 :: Named (b -> c -> a)
                                                                              , rev2 :: Named (b -> c -> a -> (b, c))
                                                                              , arg2_0 :: V b, arg2_1 :: V c }

noRev :: Named (a -> b)
noRev = Named "noRev" (\_ -> error "noRev")

data Write = forall a. (Hashable a, Typeable a) => Write (V a) a

propagateWrite :: Cache -> Write -> [Write]
propagateWrite cache write = propagateWrites cache [write]

propagateWrites :: Cache -> [Write] -> [Write]
propagateWrites cache writes = concat (map (cascade (propagateOnce cache)) writes)

propagateOnce :: Cache -> Write -> [Write]
propagateOnce cache (Write (V0 {..}) x) = []
propagateOnce cache (Write (V1 {..}) x) = [Write arg1_0 b']
  where b = r1 cache arg1_0
        b' = unName rev b x
propagateOnce cache (Write (V2 {..}) x) = [Write arg2_0 b', Write arg2_1 c']
  where b = r1 cache arg2_0
        c = r1 cache arg2_1
        (b', c') = unName rev2 b c x

-- prepareToStore :: Write -> (Key, Dynamic)
-- prepareToStore (Write v x) = (toKey v, toDyn x)

-- data WriteSet = WriteSet (M.Map Key [Write])

-- add :: WriteSet -> Write -> WriteSet
-- add (WriteSet m) write =
--   let (key, dyn) = prepareToStore write
--       existingWrites = M.findWithDefault [] key m
--       m' = M.insert key (existingWrites ++ [dyn]) m
--    in WriteSet m'

-- propagateWrite :: WriteSet -> Write -> WriteSet

hws :: Hashable a => Int -> a -> Int
hws salt x = hash (salt, hash x)

instance Hashable a => Hashable (V a) where
  hash V0 {..} = hash val
  hash V1 {..} = hash (for, rev, arg1_0)
  hash V2 {..} = hash (for2, rev2, arg2_0, arg2_1)
  hashWithSalt = hws

instance Hashable a => Keyable (V a)

data Cache = Cache (M.Map Key Dynamic)
  deriving Show

readCache :: Typeable a => Cache -> Key -> Maybe a
readCache (Cache m) k = case m M.!? k of Just dyn -> fromDynamic dyn
                                         Nothing -> Nothing

r1 :: (Hashable b, Typeable b) => Cache -> V b -> b
r1 cache v =
  case readCache cache (toKey v) of Just x -> x
                                    Nothing -> applyV cache v

applyV :: Cache -> V b -> b
applyV cache (V0 {..}) = val
applyV cache (V1 {..}) = unName for (r1 cache arg1_0)
applyV cache (V2 {..}) = unName for2 (r1 cache arg2_0) (r1 cache arg2_1)

--applyVRev :: Cache -> V b -> b -> 

incer :: Named (Int -> Int)
incer = Named "incer" (+1)

anInt :: V Int
anInt = V0 { val = 10 }

twelve :: V Int
twelve = V0 { val = 12 }

nextInt :: V Int
nextInt = V1 { for = incer, rev = noRev, arg1_0 = anInt }

showN :: Show a => Named (a -> String)
showN = Named "show" show

showNextInt :: V String
showNextInt = V1 { for = showN, rev = noRev, arg1_0 = nextInt }

plusN :: Named (Int -> Int -> Int)
plusN = Named "+" (+)

aSum :: V Int
aSum = V2 { for2 = plusN, rev2 = noRev, arg2_0 = nextInt, arg2_1 = twelve }

emptyCache :: Cache
emptyCache = Cache M.empty

-- uses the empty cache to evaluate, not the passed-in one
seedCache :: (Typeable a, Hashable a) => Cache -> V a -> Cache
seedCache (Cache m) v = Cache m'
  where m' = M.insert (toKey v) (toDyn (applyV emptyCache v)) m

theCache :: Cache
theCache = seedCache emptyCache twelve

main = do
  msp $ r1 theCache anInt
  msp $ r1 theCache nextInt
  msp $ r1 theCache twelve
  msp $ r1 theCache showNextInt
  msp $ r1 theCache aSum
  msp "hi"

{-

data NC1 a b  = NC1 { nodeName :: String
                    , for :: a -> b
                    , rev :: a -> b -> a }

data N1 a b = N1 { nc1 :: NC1 a b, arg0 :: a }

type V b = NC1 W b

r :: V b -> W -> b
r (NC1 { for }) w = for w

incer :: NC1 Int Int
incer = NC1 { nodeName = "adder"
            , for = (+1)
            , rev = \_ x -> x-1 }

appender :: NC1 String String
appender = NC1 { nodeName = "appender"
               , for = (++ "x")
               , rev = rev' }
  where rev' _ y | last y == 'x' = init y
                 | otherwise = error "appender"

data W = W { anInt :: Int
           , aString :: String }
w :: W
w = W { anInt = 10
      , aString = "asdf" }

--_anInt :: NC1 W Int
_anInt :: V Int
_anInt = NC1 { nodeName = "_anInt"
             , for = anInt
             , rev = \w i -> w { anInt = i } }

_aString :: V String
_aString = NC1 { nodeName = "_aString"
               , for = aString
               , rev = \w s -> w { aString = s } }

--anIntInc :: NC1 W Int
anIntInc :: V Int
anIntInc = applyNC1 incer _anInt

aStringAppended :: V String
aStringAppended = applyNC1 appender _aString

applyNC1 :: NC1 b c -> NC1 a b -> NC1 a c
applyNC1 (NC1 nameBC forBC revBC) (NC1 nameAB forAB revAB) = NC1 nameAC forAC revAC
  where nameAC = nameBC ++ "+" ++ nameAB
        forAC = forBC . forAB
        -- revAC :: a -> c -> a
        revAC = \a c -> revAB a (revBC (forAB a) c)
        -- a -> b
        -- a -> b -> a
        -- b -> c -> b

_main = do
  msp $ r _anInt w
  msp $ r anIntInc w
  msp $ r (applyNC1 incer anIntInc) w
  msp $ r aStringAppended w
  msp "hi"
  -}
