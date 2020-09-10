--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Data.Dynamic
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
--import GHC.Generics hiding (V1)
import qualified Debug.Trace as TR

import Hash
import Util

{-

Punt: aside from the working stuff at the top here, below is a series of
increasingly misguided attempts to try to avoid having to treat different arity
nodes differently.

-}

data W = W { anInt :: Int, aString :: String }
  deriving (Show, Typeable)
instance Nice W
instance Nice Int
instance Nice String

-- Contains a hash or fake hash
data Key = Key String
  deriving (Show, Eq, Ord)

class Keyable a where
  toKey :: a -> Key

-- compositeKey :: Keyable a => [a] -> Key
-- compositeKey keyables = Key $ hash $ concat $ map (\(Key s) -> s) $ map toKey keyables
compositeKey :: [Key] -> Key
compositeKey keys = Key $ hash $ concat $ map (\(Key s) -> s) keys

-- Values amenable to TMI.
-- Show in here for development, should remove
class (Show a, Typeable a) => Nice a

data ValueCache = ValueCache (M.Map Key Dynamic)
readCache :: Nice a => ValueCache -> V a -> a
readCache (ValueCache m) v = fromJust $ fromDynamic (fromJust (m M.!? (getKey v)))
writeCache :: Nice a => ValueCache -> V a -> a -> ValueCache
writeCache (ValueCache m) v x = ValueCache m'
  where m' = M.insert (getKey v) (toDyn x) m
emptyValueCache = ValueCache M.empty

data Write = forall a. Show a => Write (V a) a
instance Show Write where
  show (Write v x) = "(Write " ++ show v ++ " " ++ show x ++ ")"
data DW = DW [Write]
  deriving Show

-- Need to support:
-- - give me a list of your arguments (as Keys)
-- - please pull values for your arguments from this map/fn, compute your output, and return it
-- - please pull values for your arguments from this map/fn, take a new output to compute new values for your arguments, and return those wrapped as writes
data V a = forall args. V { getKey :: Key
                          , getArgKeys :: [Key]
                          , runForward :: ValueCache -> a
                          , runReverse :: ValueCache -> a -> DW }
instance Show (V a) where
  show (V {..}) = "(V " ++ show getKey ++ " " ++ show getArgKeys ++ ")"
instance Keyable (V a) where
  toKey v = getKey v

-- This is the only way to make a V without applying an F to an existing one
makeRoot :: W -> V W
makeRoot w =
  -- Recursive use of this v
  let v = V { getKey = worldKey
            , getArgKeys = []
            , runForward = \cache -> readCache cache v
            , runReverse = \cache w -> DW [Write v w] }
   in v
worldKey :: Key
worldKey = Key (hash "")  -- md5 hash of empty string will probably not collide with any V

data F a b = F String (a -> b) (a -> b -> a)
instance Keyable (F a b) where
  toKey (F name _ _) = Key (hash name)

data F2 a b c = F2 String (a -> b -> c) (a -> b -> c -> (a, b))
instance Keyable (F2 a b c) where
  toKey (F2 name _ _) = Key (hash name)

lift :: (Nice a, Nice b) => F a b -> (V a -> V b)
lift f@(F _ for rev) va = V { getKey = compositeKey [toKey f, toKey va]
                              , getArgKeys = [getKey va]
                              , runForward
                              , runReverse }
  where runForward cache = let a = readCache cache va
                            in for a
        runReverse cache b' = let a = readCache cache va
                                  a' = rev a b'
                               in DW [Write va a']

lift2 :: (Nice a, Nice b) => F2 a b c -> (V a -> V b -> V c)
lift2 f@(F2 _ for rev) va vb = V { getKey = compositeKey [toKey f, toKey va, toKey vb]
                                 , getArgKeys = [getKey va, getKey vb]
                                 , runForward
                                 , runReverse }
  where runForward cache = let a = readCache cache va
                               b = readCache cache vb
                            in for a b
        runReverse cache c' = let a = readCache cache va
                                  b = readCache cache vb
                                  (a', b') = rev a b c'
                               in DW [Write va a', Write vb b']

_anInt :: V W -> V Int
_anInt = lift $ F "anInt" anInt anInt_r
  where anInt_r w i = w { anInt = i }

main = do
  let world :: W
      world = W { anInt = 12, aString = "asdf" }
  let vw = makeRoot world
      cache = writeCache emptyValueCache vw world
      vai = _anInt vw
  msp $ runForward vai cache 
  msp $ runReverse vai cache 120
  msp "hi"

{-
data V b where
  Root :: V W  -- Technically, the only value that just exists, that isn't produced somehow
  Const :: a -> V a  -- For globals/constants that spiritually exist in the world but are just constant haskell values
  App :: (Show a, Show b) => V (F a b) -> V a -> V b  -- Produce a value from a previous value

app :: (Show a, Show b) => V (F a b) -> V a -> V b
app = App

argh :: Show a => V (V a) -> V a
argh x | TR.trace (show ("argh", x)) False = undefined
argh x = undefined
-- (App (Const (Fun bother)) (App (Const (Fun _anInt)) (W)))

instance Show a => Show (V a) where
  show (Const x) = "(Const " ++ show x ++ ")"
  show (App vf va) = "(App " ++ (show vf) ++ " " ++ (show va) ++ ")"
  show Root = "(W)"

data Write = forall a. Write (V a) a
data DW = DW [Write]
instance Monoid DW where
  mempty = DW []
instance Semigroup DW where
  DW ws0 <> DW ws1 = DW (ws0 ++ ws1)

data F a b = F String (a -> b) (a -> b -> DW)
instance Show (F a b) where
  show (F name _ _) = "(Fun " ++ name ++ ")"

infixr 9 -->
type (-->) a b = V (F a b)

-- TODO can lift2 be written in terms of lift?
lift :: String -> (a -> b) -> (a -> b -> DW) -> (a --> b)
lift name f r = Const (F name f r)
-- Not quite, this should be a --> b --> c
lift2 :: String -> (a -> b -> c) -> (a -> b -> c -> DW) -> (a --> (F b c))
lift2 name f r = Const (F name
                          (\a -> (F (name ++ "'2")
                                    (\b -> f a b)
                                    (\b -> r a b)))
                          undefined)
--lift2 name f r = Const (F name (\a -> (F (name ++ "'2") (\b -> f a b) r)))

vworld :: V W
vworld = Root

_anInt :: W --> Int
_anInt = lift "_anInt" anInt undefined

__anInt :: V Int
__anInt = app _anInt vworld

_aString :: W --> String
_aString = lift "_aString" aString undefined

__aString :: V String
__aString = app _aString vworld

incer :: Int --> Int
incer = lift "incer" (+1) (\_ i -> DW 

__nextInt :: V Int
__nextInt = app incer __anInt

-- Not quite, this should be Int --> String --> String
bother :: Int --> (F String String)
bother = lift2 "bother" (\i s -> (show i) ++ s)
rah = app bother __anInt

both :: V String
both = app (app bother __anInt) __aString

r :: W -> V a -> a
r w (Const x) = x
r w Root = w
--r w (App (F f) v) = f (r w v)
r w (App vf vx) = case r w vf of F _ f _ -> f (r w vx)
--r :: V a -> a
--r (Const x) = x
----r w (App (F f) v) = f (r w v)
--r (App vf vx) = case r vf of F _ f -> f (r vx)

main = do
  let world :: W
      world = W { anInt = 12, aString = "asdf" }
  msp $ r world __anInt
  msp $ r world __nextInt
  msp $ r world __aString
  msp $ r world both
  msp "hi"

{-
-- data F0 a = F0 a
-- data F1 a b = F1 (Named (a -> b)) (Named (a -> b) -> a)
-- data F2 a b c = F2 (Named (a -> b -> c)) (Named (a -> b -> c -> (a, b)))
-- data VV a where
--   VV0 :: F0 a -> VV a
--   VV1 :: F1 a b -> VV a -> VV b
--   VV2 :: F2 a b c -> VV a -> VV b -> VV c

-- Collection of Writes?
data DW = DW [Write]
data Write = forall a. Write (VV a) a

class Lifty a where
  type Lifted a

data Single a = Single a
  deriving Show
instance Lifty (Single a) where
  type Lifted (Single a) = VV a
instance Lifty (a, b) where
  type Lifted (a, b) = (VV a, VV b)

-- No, don't use DW here, just return the values; the caller can pack them into a DW
data FF i o where
  --FF0 :: String -> (() -> a) -> (a -> () -> DW) -> FF () a
  -- FF0 :: String -> FF () a
  FF1 :: String -> ((a) -> b) -> (b -> (a) -> DW) -> FF (Single a) b
  FF2 :: String -> ((a, b) -> c) -> (c -> (a, b) -> DW) -> FF (a, b) c

data W = W { anInt :: Int, aString :: String }
world :: W
world = W { anInt = 12, aString = "asdf" }

-- I want e.g. i to be (b, c), but the thing at the end is (VV b, VV c).
-- Maybe a type family?
data VV a = forall i. App (FF i a) (Lifted i) | Base a

worldVV :: VV W
worldVV = Base world

ten :: VV Int
ten = Base 10
-- -- Gonna leave this undefined rev because I'm not sure that a constant that
-- -- doesn't come from the World actually exists?
-- ten = VV (FF0 "ten" (\() -> (10::Int)) undefined) ()

_anInt :: FF (Single W) Int
_anInt = FF1 "_anInt" anInt undefined

__anInt :: VV Int
__anInt = App _anInt worldVV

incer :: FF (Single Int) Int
incer = FF1 "incer" (+1) undefined

__nextInt :: VV Int
__nextInt = App incer __anInt

_aString :: FF (Single W) String
_aString = FF1 "_aString" aString undefined

__aString :: VV String
__aString = App _aString worldVV

both :: VV String
both = App (FF2 "both" (\(i, s) -> show i ++ s) undefined) (__anInt, __aString)

-- This doesn't use the w param!
r :: W -> VV a -> a
--r w (VV (FF0 _) i) = r w i
r w (Base a) = a
r w (App (FF1 _ for rev) iv) = for (r w iv)
r w (App (FF2 _ for rev) (iva, ivb)) = for ((r w iva), (r w ivb))

main = do
  msp $ r world ten
  msp $ r world __anInt
  msp $ r world __nextInt
  msp $ r world __aString
  msp $ r world both
  msp "hi"
-}

{-
{-
infixr 9 -->
data (-->) a b = F { for :: a -> b
                   , rev :: a -> b -> a }
arity2 :: a --> b --> c
arity2 :: a --> (b --> c)
arity2 = F { for :: a -> (b --> c)
           , rev :: a -> (b --> c) -> a  -- actual
           , rev :: a -> b -> c -> (a, b)  -- desired
           , rev :: a -> (b -> c -> b) -> a  -- just the rev part, we're getting a b and an a here, sorta (a b)

-- What we want
whatWeWant :: (a -> b -> c -> (a, b)) -> (a -> (b --> c) -> a, b -> c -> b)

b --> c is
b -> c
b -> c -> b

Given
(a -> b -> c -> (a, b))
If we are given an a, we have
(b -> c -> (a, b))
We can discard the output a and then have
(b -> c -> b)

The forward direction is easy; given
a -> b -> c
And an a, we get
b -> c

The remaining mystery is how to get a -> b -> a from this:
(a -> b -> c -> (a, b))
We need a c; once we have that we can just drop the output b (just like we discared the output a, earlier).
And what we actually have for the first rev is
a -> (b --> c) -> a
And we want a -> b -> a.
but this
a -> (b --> c) -> a
is really this
a -> (b -> c, b -> c -> b) -> a

We do write a c to the second stage, and with b->c->b we can get a b that we need for a->b->a

The first stage isn't really bidirectional; we aren't writing a modified bidi
to it, but that's what the type looks like, if this is to be compositional and
symmetrical.

Once we've partially applied to a, we have something that no longer has a in it
-- it's not possible to expose a write to that. This is solved using
-- existentials: a Write has no paramters, and that seems to work fine.

The reverse type could be changed from a->b->a to a->b->[Write], in which case
we can hide writes of other types in there too. We're basically doing that now.

a -> b
a -> b -> DW -- DW == [Write]
a -> (b -> c)
-}

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

-- -- Combine just the bidi into one thing
-- data F1 b a = F1 { for' :: Named (b -> a), rev' :: Named (b -> a -> b) }
-- -- apply-like
-- uhpply :: (Show b, Keyable a, Typeable b) => F1 b a -> V b -> V a
-- uhpply (F1 {..}) vb = v1 for' rev' vb
-- -- make it function-like
-- liftF1 :: (Show b, Keyable a, Typeable b) => F1 b a -> (V b -> V a)
-- liftF1 = uhpply

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
propagateWrites cache writes = concat (map (cascade (propagateOnce cache)) writes)

propagateOnce' :: Cache -> Write -> [Write]
propagateOnce' c w = propagateOnce c w

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
-}
-}
