--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Dynamic
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
--import GHC.Generics hiding (V1)

import Hash
import Util

data Key = Key String
  deriving (Eq, Ord, Show)

class Show a => Keyable a where
  toKey :: a -> Key
  toKey = Key . hash

combineKeys :: Key -> Key -> Key
combineKeys (Key x) (Key y) = Key (hash $ x ++ y)

instance (Keyable a, Keyable b) => Keyable (a, b) where
  toKey (x, y) = (toKey x) `combineKeys` (toKey y)
instance (Keyable a, Keyable b, Keyable c) => Keyable (a, b, c) where
  toKey (x, y, z) = (toKey x) `combineKeys` (toKey y) `combineKeys` (toKey z)
instance (Keyable a, Keyable b, Keyable c, Keyable d) => Keyable (a, b, c, d) where
  toKey (x, y, z, w) = (toKey x) `combineKeys` (toKey y) `combineKeys` (toKey z) `combineKeys` (toKey w)

instance Keyable Int
--instance Keyable String

data Named a = Named String a
unName :: Named a -> a
unName (Named _ x) = x
nameOf :: Named a -> String
nameOf (Named s _) = s

instance Show (Named a) where
  show (Named s _) = "(Named " ++ s ++ ")"

instance Keyable (Named a) where
  toKey (Named s _) = Key $ hash $ "(Named " ++ s ++ ")"

-- Show here is for development, it should be removed.
data V a = V0 { key0 :: Key
              , val :: a  }
         | forall b. (Show b, Typeable b) =>
           V1 { key1 :: Key
              , for :: Named (b -> a)
              , rev :: Named (b -> a -> b)
              , arg1_0 :: V b }
         | forall b c. (Show b, Typeable b, Show c, Typeable c) =>
           V2 { key2 :: Key
              , for2 :: Named (b -> c -> a)
              , rev2 :: Named (b -> c -> a -> (b, c))
              , arg2_0 :: V b, arg2_1 :: V c }

getKey :: V a -> Key
getKey V0 {..} = key0
getKey V1 {..} = key1
getKey V2 {..} = key2

v0 :: Keyable a => a -> V a
v0 x = V0 { key0 = toKey x, val = x }
v1 :: (Show b, Typeable b, Keyable a) => Named (b -> a) -> Named (b -> a -> b) -> V b -> V a
v1 f r b = V1 { key1 = toKey (f, r, b), for = f, rev = r, arg1_0 = b }
v2 :: (Show b, Typeable b, Show c, Typeable c, Keyable a) => Named (b -> c -> a) -> Named (b -> c -> a -> (b, c)) -> V b -> V c -> V a
v2 f r b c = V2 { key2 = toKey (f, r, b, c), for2 = f, rev2 = r, arg2_0 = b, arg2_1 = c }

instance Show a => Show (V a) where
  show (V0 {..}) = "(" ++ (show key0) ++ " " ++ (show val) ++ ")"
  show (V1 {..}) = "(" ++ (show key1) ++ " " ++ (nameOf for) ++ "/" ++ (nameOf rev) ++ " " ++ (show arg1_0) ++ ")"
  show (V2 {..}) = "(" ++ (show key2) ++ " " ++ (nameOf for2) ++ "/" ++ (nameOf rev2) ++ " " ++ (show arg2_0) ++ " " ++ (show arg2_1) ++ ")"

noRev :: Named (a -> b)
noRev = Named "noRev" (\_ -> error "noRev")

data Write = forall a. (Show a, Typeable a) => Write (V a) a

instance Show Write where
  --show (Write v x) = "Write " ++ (show v) ++ " " ++ (show x) ++ ")"
  show (Write v x) = "Write " ++ (show v) ++ " " ++ (show x)

propagateWrite :: Cache -> Write -> [Write]
propagateWrite cache write = propagateWrites cache [write]

propagateWrites :: Cache -> [Write] -> [Write]
--propagateWrites cache writes = concat (map (cascade (propagateOnce cache)) writes)
propagateWrites cache (w:ws) = eesp ("lol", propagateOnce' cache w) $ (w : (eeesp "jfc" bbb))
  where bbb = eesp "wtf" $ propagateWrites cache $ eeesp "SHIT" ((propagateOnce' cache w) ++ ws)
propagateWrites cache [] = eeesp "gov" []

propagateOnce' :: Cache -> Write -> [Write]
propagateOnce' c w = eeesp ("um", w) $ propagateOnce c $ eesp "zxcv" w

propagateOnce :: Cache -> Write -> [Write]
propagateOnce cache (Write (V0 {..}) x) = []
propagateOnce cache (Write (V1 {..}) x) = [Write arg1_0 b']
  where b = r1 cache arg1_0
        b' = unName rev b x
propagateOnce cache (Write (V2 {..}) x) = [Write arg2_0 b', Write arg2_1 c']
  where b = r1 cache arg2_0
        c = r1 cache arg2_1
        (b', c') = unName rev2 b c x

instance (Show a) => Keyable (V a)

data Cache = Cache (M.Map Key Dynamic)
  deriving Show

readCache :: Typeable a => Cache -> Key -> Maybe a
readCache (Cache m) k = case m M.!? k of Just dyn -> fromDynamic dyn
                                         Nothing -> Nothing

r1 :: (Show b, Typeable b) => Cache -> V b -> b
r1 cache v =
  case readCache cache (getKey v) of Just x -> x
                                     Nothing -> applyV cache v

applyV :: Cache -> V b -> b
applyV cache (V0 {..}) = val
applyV cache (V1 {..}) = unName for (r1 cache arg1_0)
applyV cache (V2 {..}) = unName for2 (r1 cache arg2_0) (r1 cache arg2_1)

--applyVRev :: Cache -> V b -> b -> 

incer :: Named (Int -> Int)
incer = Named "incer" (+1)
incer_r = Named "incer_r" (\_ x -> x-1)

anInt :: V Int
anInt = v0 10

twelve :: V Int
twelve = v0 12

nextInt :: V Int
nextInt = v1 incer incer_r anInt

nextNextInt :: V Int
nextNextInt = v1 incer incer_r nextInt

showN :: Show a => Named (a -> String)
showN = Named "show" show

-- showNextInt :: V String
-- showNextInt = v1 showN noRev nextInt

plusN :: Named (Int -> Int -> Int)
plusN = Named "+" (+)

aSum :: V Int
aSum = v2 plusN noRev nextInt twelve

emptyCache :: Cache
emptyCache = Cache M.empty

-- uses the empty cache to evaluate, not the passed-in one
seedCache :: (Show a, Typeable a) => Cache -> V a -> Cache
seedCache (Cache m) v = Cache m'
  where m' = M.insert (getKey v) (toDyn (applyV emptyCache v)) m

theCache :: Cache
theCache = seedCache emptyCache twelve

-- for writes
cache2 :: Cache
cache2 = seedCache (seedCache emptyCache anInt) nextInt

main = do
  msp $ r1 cache2 anInt
  msp $ r1 cache2 nextInt
  msp $ r1 cache2 nextNextInt
  msp $ propagateOnce cache2 (Write anInt 110)
  msp $ propagateOnce cache2 (Write nextInt 111)
  msp $ propagateOnce cache2 (Write nextNextInt 112)
  msp $ propagateWrite cache2 (Write nextNextInt 113)

  -- msp $ r1 theCache anInt
  -- msp $ r1 theCache nextInt
  -- msp $ r1 theCache twelve
  -- msp $ r1 theCache showNextInt
  -- msp $ r1 theCache aSum
  msp "hi"
