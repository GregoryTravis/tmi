{-# Language GADTs, NamedFieldPuns #-}

module Log
( logMain
) where

import Data.Dynamic
import Data.Maybe (catMaybes)

import Q
import Qeq
import Storage
import Ty
import Util

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

recon :: String -> Dynamic
recon "aa" = toDyn (QNamed "aa" aa)
recon "aa_" = toDyn (QNamed "aa_" aa_)
recon "bb" = toDyn (QNamed "bb" bb)
recon "bb_" = toDyn (QNamed "bb_" bb_)
recon "inc" = toDyn (QNamed "inc" inc)
recon "inc_" = toDyn (QNamed "inc_" inc_)
recon "bplus" = toDyn (QNamed "bplus" bplus)
recon "bplus_" = toDyn (QNamed "bplus_" bplus_)
recon s = error $ show ("recon", s)

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

lift1 :: (Typeable a, Typeable b) => Bi (a -> b) (a -> R a -> R b) -> Q a -> Q b
lift1 bi qa = QBiSeal (BiApp bi qa)

lift2 :: (Typeable a, Typeable b, Typeable c) => Bi (a -> b -> c) (a -> R a -> b -> R b -> R c) -> Q a -> Q b -> Q c
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

roundTrip :: Typeable a => Q a -> IO [Q a]
roundTrip q = do
  let s = qs q
      ss = show s
      rs = read ss
      q' = sq recon rs
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
  -- Works
  msp $ propToRoots theWorld (Write added 140)
  msp $ propToRoots theWorld (Write added' 140)
  msp $ propToRoots theWorld (Write added'' 140)
  msp $ propToRoots theWorld (Write added''' 140)
  roundTrip QRoot
  roundTrip added
  roundTrip added'
  roundTrip added''
  roundTrip added'''

  msp $ added == added
  msp $ added == added'
  msp $ added' == added'
  msp $ added' == added

  msp "log hi"
