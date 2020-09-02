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
import qualified Debug.Trace as TR
--import GHC.Generics hiding (V1)

import Util

data Key = Key Int deriving (Eq, Ord, Show)

class Hashable a => Keyable a where
  toKey :: a -> Key
  toKey x | TR.trace "toKey" False = undefined
          | TR.trace (show ("toKey'", hash x)) False = undefined
          | otherwise = (Key . hash) x

instance Keyable Int

data NamedFunction a b = NamedFunction String (a -> b)
  --deriving Generic
nfApply :: NamedFunction a b -> a -> b
nfApply (NamedFunction _ f) = f
nameOf :: NamedFunction a b -> String
nameOf (NamedFunction s _) = s

instance Hashable (NamedFunction a b) where
  hash (NamedFunction s _) | TR.trace (show ("vog", s)) False = undefined
                           | otherwise = hash s
  hashWithSalt salt nf | TR.trace (show ("h1", nameOf nf)) False = undefined
                       | otherwise = hash (salt, hash nf)

--data Voo b = forall a. Hashable a => Voo a | forall a. Hashable a => Voo' a a
--data Voo b = forall a. (Hashable a, Typeable a) => Voo { for' :: (a, b) }

data V b = V0 { val :: b  }
         | forall a. (Hashable a, Typeable a) => V1 { for :: NamedFunction a b, arg1_0 :: V a }
  --deriving Generic

--deriving instance Generic (V1 a)

hws :: Hashable a => Int -> a -> Int
hws salt x = hash (salt, hash x)

instance Hashable a => Hashable (V a) where
  hash V0 {..} = hash val
  hash V1 {..} = hash (for, arg1_0) -- (87::Int, for, hash arg1_0) -- hash (for, arg1_0)
  hashWithSalt = hws
  -- hash V1 {..} | TR.trace (show ("h0", nameOf for)) False = undefined
  --              | otherwise = hash (for, arg1_0)
  -- hashWithSalt salt v@(V1 { for }) | TR.trace (show ("h2", nameOf for)) False = undefined
  --                                  | otherwise = hash (salt, v)

instance Hashable a => Keyable (V a)

data Cache = Cache (M.Map Key Dynamic)

readCache :: Typeable a => Cache -> Key -> Maybe a
readCache (Cache m) k = case m M.!? k of Just dyn -> fromJust $ fromDynamic dyn
                                         Nothing -> Nothing

-- readCacheS :: (Show a, Typeable a) => Cache -> Key -> Maybe a
-- readCacheS = readCache

r1 :: (Hashable b, Typeable b) => Cache -> V b -> b
r1 cache v | TR.trace "r1" False = undefined
           ----    | TR.trace (show ("r1'", toKey v)) False = undefined
           | otherwise =
  case readCache cache (toKey v) of Just x -> x
                                    Nothing -> applyV cache v
  -- case cache M.!? (toKey v) of Just dyn -> fromJust $ fromDynamic dyn
  --                              Nothing -> apply1 v

applyV :: Cache -> V b -> b
applyV cache (V0 {..}) = val
applyV cache (V1 {..}) = nfApply for (r1 cache arg1_0)

incer :: NamedFunction Int Int
incer = NamedFunction "incer" (+1)

anInt :: V Int
-- anInt = V1 { for = NamedFunction "10" (\() -> 10), arg1_0 = error "10" }
anInt = V0 { val = 10 }

twelve :: V Int
twelve = V0 { val = 12 }

nextInt :: V Int
nextInt = V1 { for = incer, arg1_0 = anInt }

emptyCache :: Cache
emptyCache = Cache M.empty

theCache :: Cache
theCache = Cache $ M.fromList
  [] --  (toKey twelve, toDyn $ applyV emptyCache twelve) ] -- (toKey anInt, toDyn (10::Int)) ]

main = do
  msp $ r1 theCache anInt
  msp $ r1 theCache nextInt
  msp $ toKey twelve
  msp $ applyV emptyCache twelve
  let cc = Cache $ M.fromList [(toKey twelve, toDyn $ applyV emptyCache twelve)]
  msp $ case cc of Cache m -> M.keys m
  msp $ case cc of Cache m -> M.elems m
  msp $ case cc of Cache m -> m M.!? (toKey twelve)
  msp $ case cc of Cache m -> ((fromDynamic $ fromJust $ m M.!? (toKey twelve)) :: Maybe Int)
  msp $ r1 cc anInt
  msp $ r1 emptyCache nextInt
  msp $ r1 cc nextInt
  --msp $ readCache theCache (toKey anInt)
  -- let x :: Int
  --     x = fromJust $ readCache theCache (toKey anInt)
  -- let j :: Dynamic
  --     j = fromJust $ case theCache of Cache m -> m M.!? (toKey anInt)
  -- let x :: Int
  --     x = fromJust $ fromDynamic j
  -- msp $ toKey anInt
  --msp x
  --msp $ toKey (3::Int)
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
