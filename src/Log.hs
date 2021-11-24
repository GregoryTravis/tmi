{-# Language AllowAmbiguousTypes, ExistentialQuantification, KindSignatures, GADTs, NamedFieldPuns,
    RankNTypes, ScopedTypeVariables, PartialTypeSignatures, TypeApplications, TypeOperators #-}

module Log
( logMain
) where

import Data.Dynamic
import Data.Kind (Type)
import Data.Proxy
import Data.Maybe (fromJust, catMaybes)
import Type.Reflection

import Util

reconstituteShowD :: String -> Dynamic
reconstituteShowD "aa" = toDyn (QNamed "aa" aa)
reconstituteShowD "aa_" = toDyn (QNamed "aa_" aa_)
reconstituteShowD "bb" = toDyn (QNamed "bb" bb)
reconstituteShowD "bb_" = toDyn (QNamed "bb_" bb_)
reconstituteShowD "inc" = toDyn (QNamed "inc" inc)
reconstituteShowD "inc_" = toDyn (QNamed "inc_" inc_)
reconstituteShowD "bplus" = toDyn (QNamed "bplus" bplus)
reconstituteShowD "bplus_" = toDyn (QNamed "bplus_" bplus_)
reconstituteShowD s = error $ show ("recon", s)

data W = W { aa :: Int, bb :: Int } deriving (Read, Show)

data Write = forall a. Write (Q a) a | Writes [Write]
emptyWrite :: Write
emptyWrite = Writes []

propWrite :: W -> Write -> Write
propWrite w (Write qa a) = wr w qa a

propWriteSome :: W -> Write -> [Write]
propWriteSome w (Write qa a) = [wr w qa a]
propWriteSome w (Writes ws) = concat $ map (propWriteSome w) ws

propWriteFully :: W -> Write -> [Write]
propWriteFully w write@(Write QRoot _) = [write]
propWriteFully w write = write : (concat $ map (propWriteFully w) (propWriteSome w write))

propToRoots :: W -> Write -> [W]
propToRoots w write =
  let writes = propWriteFully w write
   in catMaybes $ map ifRoot writes

ifRoot :: Write -> Maybe W
ifRoot (Write QRoot w) = Just w
ifRoot _ = Nothing

instance Semigroup Write where
  w <> w' = Writes [w, w']

instance Show Write where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (Writes ws) = show ws
data R a = R (a -> Write)

data Bi f r where
  Bi :: Q f -> Q r -> Bi f r
  -- BiApp :: Q (a -> b) -> Q (a -> R a -> c) -> Q a -> Bi b c
  BiApp :: Bi (a -> b) (a -> R a -> c) -> Q a -> Bi b c

data Q a where
  QRoot :: Q W
  QNice :: (Show a, Read a, Typeable a) => a -> Q a
  QNamed :: String -> a -> Q a
  QApp :: Q (a -> b) -> Q a -> Q b
  QBiSeal :: Bi a (R a) -> Q a

sepp :: Bi (Int -> Int -> Int)
           (Int -> Log.R Int -> Int -> Log.R Int -> Log.R Int)
sepp = Bi (QNamed "bplus" bplus) (QNamed "bplus_" bplus_)
sepp2 :: Bi (Int -> Int) (Int -> Log.R Int -> Log.R Int)
sepp2 = BiApp sepp baa
sepp3 :: Bi Int (Log.R Int)
sepp3 = BiApp sepp2 bbb
sepp3s :: Q Int
sepp3s = QBiSeal sepp3

sepp3s' = QBiSeal (BiApp (BiApp sepp baa) bbb)

plurs :: Bi (Int -> Int)
            (Int -> Log.R Int -> Log.R Int)
plurs = Bi (QNamed "inc" inc) (QNamed "inc_" inc_)

sepp3s'' = QBiSeal (BiApp (BiApp sepp (QBiSeal (BiApp plurs baa))) bbb)
sepp3s''' = QBiSeal (BiApp (BiApp sepp baa) (QBiSeal (BiApp plurs bbb)))
sepp3s'''' = QBiSeal (BiApp (BiApp sepp (QBiSeal (BiApp plurs baa))) (QBiSeal (BiApp plurs bbb)))

bplus :: Int -> Int -> Int
bplus = (+)
bplus_ :: Int -> R Int -> Int -> R Int -> R Int
bplus_ _ (R ra) _ (R rb) = R rc
  where rc c = let a = c `div` 2
                   b = c - a
                in ra a <> rb b

baa :: Q Int
baa = QBiSeal (BiApp (Bi (QNamed "aa" aa) (QNamed "aa_" aa_)) root)
-- BApp (QNamed "aa" aa) (QNamed "aa_" aa_) root
-- aa :: W -> Int
aa_ :: W -> R W -> R Int
aa_ w (R wr) = R ir
  where ir aa = wr $ w { aa }

bbb :: Q Int
bbb = QBiSeal (BiApp (Bi (QNamed "bb" bb) (QNamed "bb_" bb_)) root)
-- bbb = BApp (QNamed "bb" bb) (QNamed "bb_" bb_) root
-- bb :: W -> Int
bb_ :: W -> R W -> R Int
bb_ w (R wr) = R ir
  where ir bb = wr $ w { bb }

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ (R r) = R r'
  where r' i = r (i - 1)

wr :: W -> Q b -> b -> Write
wr w (QBiSeal (Bi qfor qrev)) na =
  let rev = rd w qrev -- R a
      oa = rd w qfor -- a
   in case rev of R rec -> rec na
wr w (QBiSeal bi) a =
  let R rarec = getrev w bi
   in rarec a
wr w q _ = error $ "wr " ++ show q

getrev :: W -> Bi f r -> r
getrev w (Bi qf qr) =
  let r = rd w qr
   in r
getrev w (BiApp bi qa) =
  let rev = getrev w bi
      oa = rd w qa
      ra = R (\a -> Write qa a)
      rb = rev oa ra
   in rb

infixr 4 <$$>
(<$$>) :: Q (a -> b) -> Q a -> Q b
(<$$>) = QApp

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QApp qf qx) = "(QApp " ++ show qf ++ " " ++ show qx ++ ")"
  show (QBiSeal bi) = "(QBiSeal " ++ show bi ++ ")"

instance Show (Bi f r) where 
  show (Bi qf qr) = "(Bi " ++ show qf ++ " " ++ show qr ++ ")"
  show (BiApp bi qa) = "(BiApp " ++ show bi ++ " " ++ show qa ++ ")"

rd :: W -> Q a -> a
rd w QRoot = w
rd w (QNice x) = x
rd w (QNamed _ x) = x
rd w (QApp qf qx) = rd w qf (rd w qx)
rd w (QBiSeal bi) = rdb w bi

rdb :: W -> Bi f r -> f
rdb w (Bi qf qr) = rd w qf
rdb w (BiApp bi qa) =
  let for = rdb w bi
      a = rd w qa
   in for a

root :: Q W
root = QRoot
vaa = QNamed "aa" aa
one = QNice (1::Int)
plus = QNamed "inc" (+(1::Int))
faa = vaa <$$> root
_inced = plus <$$> faa
inced = plus <$$> vaa <$$> root
w :: W
w = W { aa = 13, bb = 100 }

---- Show, Read. First is shewn, second is type
data DumDyn = DumDyn String String deriving (Read, Show)
mkDumDyn :: (Show a, Read a, Typeable a) => a -> DumDyn
mkDumDyn x = DumDyn (show x) (show (toDyn x))

dumDynToXShowD :: DumDyn -> Dynamic
dumDynToXShowD (DumDyn s ts) = dyn
  where dyn = case ts of "<<Int>>" -> toDyn $ QNice (read s :: Int)
                         "<<String>>" -> toDyn $ QNice (read s :: String)
                         _ -> error $ "dumDynToX " ++ ts

data S = SRoot | SNice DumDyn | SNamed String | SApp S S -- | BSApp S S S | BSApp2 S S S S
       | SQBiSeal BS
  deriving (Read, Show)
data BS = BSBi S S | BSBiApp BS S
  deriving (Read, Show)

qs :: Q a -> S
qs QRoot = SRoot
qs (QNice x) = SNice (mkDumDyn x)
qs (QNamed name _) = SNamed name
qs (QApp qf qx) = SApp (qs qf) (qs qx)
qs (QBiSeal bi) = SQBiSeal (bs bi)

bs :: Bi f r -> BS
bs (Bi f r) = BSBi (qs f) (qs r)
bs (BiApp bi q) = BSBiApp (bs bi) (qs q)

qapp :: Dynamic  -> Dynamic -> Maybe Dynamic
qapp (Dynamic qabt@(App q (Fun ta tr)) qf) qat@(Dynamic (App q' ta') qx)
  | Just HRefl <- q `eqTypeRep` q'
  , Just HRefl <- q `eqTypeRep` (typeRep @Q)
  , Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic (App q tr) (QApp qf qx))
qapp (Dynamic qabt _) (Dynamic qat _) =
   error $ "QApp (" ++ show qabt ++ ") (" ++ show qat ++ ") "

qbiseal :: Dynamic -> Maybe Dynamic
qbiseal (Dynamic bit@(App (App bit0 at0) (App rt0 at1)) bi)
  | Just HRefl <- bit0 `eqTypeRep` (typeRep @Bi)
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- at0 `eqTypeRep` at1
  = Just (Dynamic (App (typeRep @Q) at0) (QBiSeal bi))

bi :: Dynamic -> Dynamic -> Maybe Dynamic
bi (Dynamic (App qt0 ft0) qf)
   (Dynamic (App qt1 rt0) qr)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @Q)
  , Just HRefl <- qt0 `eqTypeRep` qt1
  = Just (Dynamic (App (App (typeRep @Bi) ft0) rt0) (Bi qf qr))

bsbiapp :: Dynamic -> Dynamic -> Maybe Dynamic
bsbiapp (Dynamic (App (App bit0 (Fun at0 bt0))
                                (Fun at1 (Fun (App rt0 at2) ct0))) bi)
        (Dynamic (App qt0 at3) q)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @Q)
  , Just HRefl <- bit0 `eqTypeRep` (typeRep @Bi)
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind bt0
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind ct0
  , Just HRefl <- at0 `eqTypeRep` at1
  , Just HRefl <- at0 `eqTypeRep` at2
  , Just HRefl <- at0 `eqTypeRep` at3
  = Just (Dynamic (App (App bit0 bt0) ct0) (BiApp bi q))

sqd :: S -> Dynamic
sqd SRoot = toDyn QRoot
sqd (SNice ddyn) = dumDynToXShowD ddyn
sqd (SNamed name) = reconstituteShowD name
sqd (SApp sf sx) =
  let dsf = sqd sf
      dsx = sqd sx
   in fromJust $ qapp dsf dsx
sqd (SQBiSeal bis) =
  let dbs = bqd bis
   in fromJust $ qbiseal dbs

-- name should be bsbd
bqd :: BS -> Dynamic
bqd (BSBi sf sr) =
  let dsf = sqd sf
      dsr = sqd sr
   in fromJust $ bi dsf dsr
bqd (BSBiApp bs s) =
  let dbs = bqd bs
      ds = sqd s
   in fromJust $ bsbiapp dbs ds

sq :: Typeable a => S -> Q a
sq s = fromJustVerbose "sq'" $ fromDynamic $ sqd s

logMain = do
  msp $ rd w sepp3s
  msp $ wr w sepp3s 140
  msp $ propWriteFully w (Write sepp3s 140)
  msp $ propToRoots w (Write sepp3s 140)
  msp $ propToRoots w (Write sepp3s' 160)
  msp $ propToRoots w (Write sepp3s'' 160)
  msp $ propToRoots w (Write sepp3s''' 160)
  msp $ propToRoots w (Write sepp3s'''' 160)
  msp $ qs QRoot
  msp $ ((sq (qs QRoot)) :: Q W)
  msp $ qs sepp3s
  msp $ ((sq (qs sepp3s)) :: Q Int)
  msp $ qs sepp3s'
  msp $ ((sq (qs sepp3s')) :: Q Int)
  msp $ qs sepp3s''
  msp $ ((sq (qs sepp3s'')) :: Q Int)
  msp $ qs sepp3s'''
  msp $ ((sq (qs sepp3s''')) :: Q Int)
  msp $ qs sepp3s''''
  msp $ ((sq (qs sepp3s'''')) :: Q Int)
  msp "log hi"
