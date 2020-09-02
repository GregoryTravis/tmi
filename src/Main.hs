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

data NamedFunction a b = NamedFunction String (a -> b)
nfApply :: NamedFunction a b -> a -> b
nfApply (NamedFunction _ f) = f
nameOf :: NamedFunction a b -> String
nameOf (NamedFunction s _) = s

data NamedFunction2 a b c = NamedFunction2 String (a -> b -> c)
nfApply2 :: NamedFunction2 a b c -> a -> b -> c
nfApply2 (NamedFunction2 _ f) = f
nameOf2 :: NamedFunction2 a b c -> String
nameOf2 (NamedFunction2 s _) = s

instance Hashable (NamedFunction a b) where
  hash (NamedFunction s _) = hash s
  hashWithSalt = hws

instance Hashable (NamedFunction2 a b c) where
  hash (NamedFunction2 s _) = hash s
  hashWithSalt = hws

--data Voo b = forall a. Hashable a => Voo a | forall a. Hashable a => Voo' a a
--data Voo b = forall a. (Hashable a, Typeable a) => Voo { for' :: (a, b) }

data V b = V0 { val :: b  }
         | forall a. (Hashable a, Typeable a) => V1 { for :: NamedFunction a b, arg1_0 :: V a }
         | forall a c. (Hashable a, Typeable a, Hashable c, Typeable c) => V2 { for2 :: NamedFunction2 a c b, arg2_0 :: V a, arg2_1 :: V c }
  --deriving Generic

--deriving instance Generic (V1 a)

hws :: Hashable a => Int -> a -> Int
hws salt x = hash (salt, hash x)

instance Hashable a => Hashable (V a) where
  hash V0 {..} = hash val
  hash V1 {..} = hash (for, arg1_0)
  hash V2 {..} = hash (for2, arg2_0, arg2_1)
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
applyV cache (V1 {..}) = nfApply for (r1 cache arg1_0)
applyV cache (V2 {..}) = nfApply2 for2 (r1 cache arg2_0) (r1 cache arg2_1)

incer :: NamedFunction Int Int
incer = NamedFunction "incer" (+1)

anInt :: V Int
anInt = V0 { val = 10 }

twelve :: V Int
twelve = V0 { val = 12 }

nextInt :: V Int
nextInt = V1 { for = incer, arg1_0 = anInt }

showN :: Show a => NamedFunction a String
showN = NamedFunction "show" show

showNextInt :: V String
showNextInt = V1 { for = showN, arg1_0 = nextInt }

plusN :: NamedFunction2 Int Int Int
plusN = NamedFunction2 "+" (+)

aSum :: V Int
aSum = V2 { for2 = plusN, arg2_0 = nextInt, arg2_1 = twelve }

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
