{-# Language AllowAmbiguousTypes, ExistentialQuantification, GADTs, RankNTypes, ScopedTypeVariables, PartialTypeSignatures, TypeApplications #-}

module Log
( logMain
) where

import Data.Dynamic
import Data.Kind (Type)
import Data.Proxy
import Data.Maybe (fromJust)
import Type.Reflection

import Util

-- todo
-- - store the dynamic arg in a string
-- - represent the (a -> b) as a looked-up function (c -> a -> b) with c Nice, and a c,
--   storing the function name and c as a string
-- - be able to to apply the latter (with withDynArg) to the former
-- - put em in a file
--
-- The first one is the non-trivial one; oh wait, we know it's Nice tho?

-- lala' :: Typeable a => (a -> b) -> (Dynamic -> b)
-- lala' = undefined

withNiceArg :: forall a b. (Show a, Read a) => (a -> b) -> (String -> b)
withNiceArg f = f . read

withDynArg :: forall a b. Typeable a => (a -> b) -> (Dynamic -> b)
withDynArg f = f'
  where f' dx = case fromDynamic dx of Just x -> f x
                                       Nothing -> error "oof"

oof = withDynArg (+(1::Int))
tlSteps = [oof]
tlVals = [toDyn (12 :: Int)]

incInt :: Int -> Int
incInt = (+1)

-- This correctly parses a nice argument and passes it to a function
-- And it works for both int and (bool, bool)
data Step = forall a b. Read a => Step (IO a, a -> IO ())

resolveStep :: String -> Step -> IO ()
resolveStep sx (Step (_, k)) = do
  let x = read sx
   in k x

moreSteps = [Step (return (False, False), msp)]
moreRets = [show (True, True)]

reconstitute :: (Show a, Read a, Typeable a) => String -> a
reconstitute "aa" = fromJust $ fromDynamic $ toDyn aa
reconstitute s = error $ show ("recon", s)

reconstituteShow :: Typeable a => String -> Q a
reconstituteShow "aa" = fromJust $ fromDynamic $ toDyn (QNamed "aa" aa)
reconstituteShow s = error $ show ("recon", s)

reconstituteShowD :: String -> Dynamic
reconstituteShowD "aa" = toDyn (QNamed "aa" aa)
reconstituteShowD s = error $ show ("recon", s)

newtype W = W { aa :: Int } deriving (Read, Show)

data Q a where
  QRoot :: Q W
  QNice :: (Show a, Read a, Typeable a) => a -> Q a
  QNamed :: String -> a -> Q a
  QApp :: Q (a -> b) -> Q a -> Q b

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QApp qf qx) = "(QApp " ++ show qf ++ " " ++ show qx ++ ")"

rd :: W -> Q a -> a
rd w QRoot = w
rd w (QNice x) = x
rd w (QNamed _ x) = x
rd w (QApp qf qx) = rd w qf (rd w qx)

root = QRoot
vaa = QNamed "aa" aa
one = QNice (1::Int)
plus = QNamed "plus" (+(1::Int))
faa = QApp vaa root
inced = QApp plus faa
w = W { aa = 13 }

-- Show, Read. First is shewn, second is type
data DumDyn = DumDyn String String deriving (Read, Show)
mkDumDyn :: (Show a, Read a, Typeable a) => a -> DumDyn
mkDumDyn x = DumDyn (show x) (show (toDyn x))

dumDynToX :: (Show a, Read a, Typeable a) => DumDyn -> a
dumDynToX (DumDyn s ts) = fromJust $ fromDynamic dyn
  where dyn = case ts of "<<Int>>" -> toDyn (read s :: Int)
                         "<<String>>" -> toDyn (read s :: String)
                         _ -> error $ "dumDynToX " ++ ts

dumDynToXShow :: (Typeable a) => DumDyn -> Q a
dumDynToXShow (DumDyn s ts) = fromJust $ fromDynamic dyn
  where dyn = case ts of "<<Int>>" -> toDyn $ QNice (read s :: Int)
                         "<<String>>" -> toDyn $ QNice (read s :: String)
                         _ -> error $ "dumDynToX " ++ ts

dumDynToXShowD :: DumDyn -> Dynamic
dumDynToXShowD (DumDyn s ts) = dyn
  where dyn = case ts of "<<Int>>" -> toDyn $ QNice (read s :: Int)
                         "<<String>>" -> toDyn $ QNice (read s :: String)
                         _ -> error $ "dumDynToX " ++ ts

data S = SRoot | SNice DumDyn | SNamed String | SApp S S
  deriving (Read, Show)

qs :: Q a -> S
qs QRoot = SRoot
qs (QNice x) = SNice (mkDumDyn x)
qs (QNamed name _) = SNamed name
qs (QApp qf qx) = SApp (qs qf) (qs qx)

data Ding = forall a. Typeable a => Ding a Dynamic (Ding -> String)
mkding :: Typeable a => a -> Ding
mkding x = Ding x (toDyn x) stuff
stuff :: Ding -> String
stuff (Ding x' (Dynamic trx xx) _) =
  let q = case splitApps (typeRep @Q) of (tycon', strs) -> tycon'
      -- App qf qa = (typeRep @(Q W)) -- ok
      -- Fun _ _ = typeRep @(Int -> String) -- ok
      -- App qq qf = typeRep @(Q (W -> Int))
      -- Fun qfa qfr = qf
      -- yeah = typeRep @(Maybe Int) == App (typeRep @Maybe) (typeRep @Int)
      App qq (Fun qfa qfr) = typeRep @(Q (W -> Int))
      yeah0 = eqTypeRep qfa (typeRep @W)
      yeah1 = eqTypeRep qfa (typeRep @Int)
      wtr' = typeRep @W
      -- yeah2 = eqTypeRep yeah0 qfa
   in show ("heh", yeah0, yeah1)
   -- in case splitApps trx
   --      of (tycon, strs) ->
   --           case strs of [SomeTypeRep (Fun tr _)] -> show (tr, tr `eqTypeRep` typeRep @W)
   -- in case splitApps trx of (tycon, strs) -> show strs -- ok
   -- in case splitApps trx of (tycon, strs) -> show (tycon == q) -- ok
  -- show trx
doding :: Ding -> String
doding d@(Ding x dx f) = f d
foo = mkding vaa
laa = QApp vaa root

-- querp :: Dynamic -> Dynamic -> String
-- querp (Dynamic qabtr qab) (Dynamic qatr qa) =
--   let (q, []) = splitApps (typeRep @Q)
--    in case splitApps qabtr of (tycon, strs) -> show (tycon, strs, eqTypeRep tycon q)

-- -- alp = \(_::S) -> toDyn $ QNice $ dumDynToX undefined
-- ooo :: Typeable a => a
-- ooo = undefined
-- -- Can't: even tho ooo is typeable, it's poly so it's not really typeable
-- larr = toDyn ooo

sqd :: S -> Dynamic
sqd SRoot = toDyn QRoot
-- sqd (SNice ddyn) = toDyn $ QNice (dumDynToX ddyn)
sqd (SNice ddyn) = dumDynToXShowD ddyn
-- sqd (SNamed name) = toDyn $ QNamed name (reconstitute name)
sqd (SNamed name) = reconstituteShowD name
-- sqd (SApp sf sx) = fromJustVerbose "sqd SApp"$ dynApply (sqd sf) (sqd sx)
-- dqs (SApp sf sx) = toDyn q
--   where q = QApp (fromJust $ fromDynamic (sqd sf)) (fromJust $ fromDynamic (sqd sx))
-- sqd (SApp sf sx) =
--   let Dynamic _ f = sf
--       Dynamic _ x = x
--    in QApp f x
-- sqd (SApp sf sx) =
--   let df = sqd sf
--       dx = sqd sx
--    in fromJust $ dynApply df dx
-- sqd _ = error "sq"

sq' :: Typeable a => S -> Q a
sq' s = fromJustVerbose "sq'" $ fromDynamic $ sqd s

-- sq :: (Show a, Read a, Typeable a) => S -> Q a
-- Can't do this because then we don't know that the arg to QNice is a show (more
-- precisely that we don't know which one, or even more precisely, we can't restrict
-- this to only be used *at some Show monotype*.
sq :: Typeable a => S -> Q a
sq SRoot = fromJustVerbose "sq SRoot" $ fromDynamic $ toDyn QRoot
sq (SNice ddyn) = dumDynToXShow ddyn -- QNice (dumDynToX ddyn)
sq (SNamed name) = reconstituteShow name -- QNamed name (reconstitute name)
-- sq (SApp sf sx) = QApp (sq sf) (sq sx)
sq _ = error "sq"

wak :: Dynamic -> Int
wak (Dynamic tr f) =
  let qwt = typeRep @(Q W) -- typeOf QRoot -- (Proxy @(Q W))
   in case tr `eqTypeRep` qwt
        of Nothing -> error ("yope " ++ show tr ++ " " ++ show qwt)
           Just HRefl -> 14

-- wuk :: Dynamic -> Dynamic -> Int
-- wuk (Dynamic qabtr qab) (Dynamic qatr qa)

-- Trying to drill down into a type but maybe you can't do it in ghci
-- :t (case (head (case splitApps (typeRep @(Q (W -> Int))) of (tycon, strs) -> strs)) of SomeTypeRep (Fun tra _) -> tra `eqTypeRep` tra) :: Maybe (arg :~~: arg)

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply' :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply' (Dynamic (Fun ta tr) f) (Dynamic ta' x)
  | Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic tr (f x))
dynApply' _ _
  = Nothing

-- Minimal example of how we can't treat a (Show a => a) as a Show
-- returnsShow :: Show a => Int -> a
-- returnsShow = undefined
-- data Thing a where
--   ShowThing :: Show a => a -> Thing a
-- foo = ShowThing (returnsShow undefined)

sfaa = qs faa
qfaa = sq' sfaa :: Q Int

logMain = do
  -- msp $ querp (toDyn vaa) (toDyn QRoot)
  msp $ doding foo

  -- works
  -- msp sfaa
  -- -- msp qfaa
  -- msp $ rd w inced
  -- msp $ rd w (sq $ SNice (DumDyn "12" "<<Int>>") :: Q Int)
  -- let foo = ((rd w (sq $ SNamed "aa" :: Q (W -> Int))) :: (W -> Int))
  -- msp $ foo w
  -- msp $ rd w (sq $ SRoot :: Q W)

  -- Can't because sq requires Show
  -- let theAA = r w $ (sq $ SNamed "aa" :: Q (W -> Int))
  -- msp $ r w $ sq $ SNice (DumDyn "<<Int>>" "13")
  -- msp $ r w qfaa
  -- works
  -- let i = 12 :: Int
  --     step = Step (return i, msp)
  --     step' = Step (return (False, True), msp)
  --     sx' = show (13 :: Int)
  --     resultAction = resolveStep sx' step
  --     resultAction' = resolveStep (show (True, False)) step'
  -- resultAction
  -- resultAction'
  -- resolveStep (head moreRets) (head moreSteps)

  -- -- Works
  -- let i = (12::Int)
  --     di = toDyn i
  --     sum = oof di
  -- msp sum
  -- msp $ head tlSteps (head tlVals)
  -- msp $ withNiceArg incInt "140"
  msp "log hi"
