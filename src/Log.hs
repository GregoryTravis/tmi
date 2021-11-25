{-# Language GADTs, NamedFieldPuns #-}

module Log
( logMain
) where

import Data.Dynamic
import Data.Maybe (fromJust, catMaybes)

import Dyn
import Ty
import Util
import Q

-- todo
-- + dead: QApp, <$$>, faa, inced_
-- + w -> theWorld (cuz it's often a param that I sometimes forget to pass)
-- + move typerep stuff to another file so we don't have to rebuild all the time
-- + lifters and use them for sepps
-- - remove most everything else
-- - Eq for Q -- need BiApp
-- - roundTrip asserts they're equal
-- - modules: propagate, serialization, rd/wr
-- - multi-module registry

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

addEmBi :: Bi (Int -> Int -> Int)
            (Int -> R Int -> Int -> R Int -> R Int)
addEmBi = Bi (QNamed "bplus" bplus) (QNamed "bplus_" bplus_)
addEm = lift2 addEmBi

plursBi :: Bi (Int -> Int)
            (Int -> R Int -> R Int)
plursBi = Bi (QNamed "inc" inc) (QNamed "inc_" inc_)
plurs = lift1 plursBi

lift1 :: Bi (a -> b) (a -> R a -> R b) -> Q a -> Q b
lift1 bi qa = QBiSeal (BiApp bi qa)

lift2 :: Bi (a -> b -> c) (a -> R a -> b -> R b -> R c) -> Q a -> Q b -> Q c
lift2 bi qa qb = QBiSeal (BiApp (BiApp bi qa) qb)

added = addEm baa bbb
added' = addEm (plurs baa) bbb
added'' = addEm baa (plurs bbb)
added''' = addEm (plurs baa) (plurs bbb)

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

rd :: W -> Q a -> a
rd w QRoot = w
rd w (QNice x) = x
rd w (QNamed _ x) = x
rd w (QBiSeal bi) = rdb w bi

rdb :: W -> Bi f r -> f
rdb w (Bi qf qr) = rd w qf
rdb w (BiApp bi qa) =
  let for = rdb w bi
      a = rd w qa
   in for a

root :: Q W
root = QRoot
theWorld :: W
theWorld = W { aa = 13, bb = 100 }

---- Show, Read. First is shewn, second is type
data DumDyn = DumDyn String String deriving (Read, Show)
mkDumDyn :: (Show a, Read a, Typeable a) => a -> DumDyn
mkDumDyn x = DumDyn (show x) (show (toDyn x))

dumDynToXShowD :: DumDyn -> Dynamic
dumDynToXShowD (DumDyn s ts) = dyn
  where dyn = case ts of "<<Int>>" -> toDyn $ QNice (read s :: Int)
                         "<<String>>" -> toDyn $ QNice (read s :: String)
                         _ -> error $ "dumDynToX " ++ ts

data S = SRoot | SNice DumDyn | SNamed String | SQBiSeal BS
  deriving (Read, Show)
data BS = BSBi S S | BSBiApp BS S
  deriving (Read, Show)

qs :: Q a -> S
qs QRoot = SRoot
qs (QNice x) = SNice (mkDumDyn x)
qs (QNamed name _) = SNamed name
qs (QBiSeal bi) = SQBiSeal (bs bi)

bs :: Bi f r -> BS
bs (Bi f r) = BSBi (qs f) (qs r)
bs (BiApp bi q) = BSBiApp (bs bi) (qs q)

sqd :: S -> Dynamic
sqd SRoot = toDyn QRoot
sqd (SNice ddyn) = dumDynToXShowD ddyn
sqd (SNamed name) = reconstituteShowD name
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

roundTrip :: Typeable a => Q a -> IO [Q a]
roundTrip q = do
  let s = qs q
      ss = show s
      rs = read ss
      q' = sq rs
      -- check = assertM "roundTrip" (q == q' && s == rs) [q, q']
      check = assertM "roundTrip" True [q, q']
  msp "===="
  msp q
  msp s
  msp ss
  msp rs
  msp q'
  msp "===="
  return check

logMain = do
  msp $ propToRoots theWorld (Write added 140)
  msp $ propToRoots theWorld (Write added' 140)
  msp $ propToRoots theWorld (Write added'' 140)
  msp $ propToRoots theWorld (Write added''' 140)
  roundTrip QRoot
  roundTrip added
  roundTrip added'
  roundTrip added''
  roundTrip added'''
  msp "log hi"
