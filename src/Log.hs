{-# Language AllowAmbiguousTypes, ExistentialQuantification, KindSignatures, GADTs, NamedFieldPuns,
    RankNTypes, ScopedTypeVariables, PartialTypeSignatures, TypeApplications, TypeOperators #-}

module Log
( logMain
) where

-- import Data.Data (DataRep(..))
import Data.Dynamic
import Data.Kind (Type)
import Data.Proxy
import Data.Maybe (fromJust)
-- import GHC.Core.TyCon (PrimRep(..))
-- import GHC.Exts (TYPE)
-- import GHC.Exts (RuntimeRep)
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
reconstitute "inc" = fromJust $ fromDynamic $ toDyn ((+1) :: Int -> Int)
reconstitute s = error $ show ("reconstitute", s)

reconstituteShow :: Typeable a => String -> Q a
reconstituteShow "aa" = fromJust $ fromDynamic $ toDyn (QNamed "aa" aa)
reconstituteShow s = error $ show ("recon", s)

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
-- _aa :: W -> Int -> W
-- -- _aa w aa = error $ "ugh " ++ show aa
-- _aa w aa = w { aa }

-- data R a where
--   RRoot :: R W
--   RNice :: Q a -> R a
--   RNamed :: Q a -> R a
--   RApp :: Q (b -> a) -> R a -> R b

-- infixr 0 -->
-- data a --> b = L (a -> b) (a -> b -> a)

-- -- L (a -> (L (b -> c) (b -> c -> b))) (a -> (L (b -> c) (b -> c -> b)) -> a)
-- goo :: a --> b --> c
-- goo = undefined

data Write = forall a. Write (Q a) a | Writes [Write]
emptyWrite :: Write
emptyWrite = Writes []

instance Semigroup Write where
  w <> w' = Writes [w, w']

instance Show Write where
  show (Write qa a) = "(Write " ++ show qa ++ {- " " ++ show a ++ -} ")"
  show (Writes ws) = show ws
data R a = R (a -> Write)

-- receive :: W -> R a -> a -> Write
-- receive w (R rec) x = rec w x

data Q a where
  QRoot :: Q W
  QNice :: (Show a, Read a, Typeable a) => a -> Q a
  QNamed :: String -> a -> Q a
  QApp :: Q (a -> b) -> Q a -> Q b
  BApp :: {- (Show a, Show b) => -} Q (a -> b) -> Q (a -> R a -> R b) -> Q a -> Q b
  BApp2 :: {- (Show a, Show b) => -} Q (a -> b -> c) -> Q (a -> R a -> b -> R b -> R c) -> Q a -> Q b -> Q c

bplus :: Int -> Int -> Int
bplus = (+)
bplus_ :: Int -> R Int -> Int -> R Int -> R Int
bplus_ _ (R ra) _ (R rb) = R rc
  where rc c = let a = c `div` 2
                   b = c - a
                in ra a <> rb b

bplusser :: Q Int -> Q Int -> Q Int
bplusser = BApp2 (QNamed "bplus" bplus) (QNamed "bplus_" bplus_)

bplussed :: Q Int
bplussed = bplusser baa bbb

baa :: Q Int
baa = BApp (QNamed "aa" aa) (QNamed "aa_" aa_) root
-- aa :: W -> Int
aa_ :: W -> R W -> R Int
aa_ w (R wr) = R ir
  where ir aa = wr $ w { aa }

bbb :: Q Int
bbb = BApp (QNamed "bb" bb) (QNamed "bb_" bb_) root
-- bb :: W -> Int
bb_ :: W -> R W -> R Int
bb_ w (R wr) = R ir
  where ir bb = wr $ w { bb }

binc :: Q Int -> Q Int
binc = BApp (QNamed "inc" inc) (QNamed "inc_" inc_)
binced :: Q Int
binced = binc baa

inc :: Int -> Int
inc = (+1)
inc_ :: Int -> R Int -> R Int
inc_ _ (R r) = R r'
  where r' i = r (i - 1)

wr :: W -> Q b -> Q b -> Write
wr w (BApp qfor qrev qx) qv =
  let nb = rd w qv
      -- for = rd w qfor
      rev = rd w qrev
      oa = rd w qx
      ra = R (\a -> Write qx a)
      R rb = rev oa ra
   in rb nb
wr w (BApp2 qfor qrev qx qy) qc =
  let nc = rd w qc
      -- for = rd w qfor
      rev = rd w qrev
      oa = rd w qx
      ob = rd w qy
      ra = R (\a -> Write qx a)
      rb = R (\b -> Write qy b)
      R rc = rev oa ra ob rb
   in rc nc

-- loft1 :: String -> (a -> b) -> (a -> b -> a) -> Q a -> Q b
-- loft1 name for rev (B qa ra) = B qb bb
--   where qb = QApp (QNamed name for) qa
--         bb = R rec
--         rec w b = receive w ra (rev (rd w qa) b)

-- linc :: Q Int -> Q Int
-- linc = loft1 "inc" (+1) inc_
--   where inc_ _ i = i - 1

-- liaa :: Q W -> Q Int
-- liaa = loft1 "aa" aa _aa
-- qaa :: Q Int
-- qaa = liaa broot
-- qaai :: Q Int
-- qaai = linc qaa

-- broot :: Q W
-- broot = B root (R (\w w' -> Write broot w'))

-- -- Assignment goes from right to left like god intended
-- wr :: W -> Q a -> Q a -> Write
-- wr w (B _ r) qx =
--   let a = rd w qx
--    in receive w r a

infixr 4 <$$>
(<$$>) :: Q (a -> b) -> Q a -> Q b
(<$$>) = QApp

instance Show (Q a) where
  show QRoot = "QRoot"
  show (QNice x) = "(QNice " ++ show x ++ ")"
  show (QNamed name _) = "(QNamed " ++ name ++ ")"
  show (QApp qf qx) = "(QApp " ++ show qf ++ " " ++ show qx ++ ")"
  show (BApp qf qr qx) = "(BApp " ++ show qf ++ " " ++ show qr ++ " " ++ show qx ++ ")"
  show (BApp2 qf qr qx qy) = "(BApp " ++ show qf ++ " " ++ show qr ++ " " ++ show qx ++
         " " ++ show qy ++ ")"

rd :: W -> Q a -> a
rd w QRoot = w
rd w (QNice x) = x
rd w (QNamed _ x) = x
rd w (QApp qf qx) = rd w qf (rd w qx)
rd w (BApp qf _ qx) = rd w qf (rd w qx)
rd w (BApp2 qf _ qx qy) = (rd w qf) (rd w qx) (rd w qy)

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

data S = SRoot | SNice DumDyn | SNamed String | SApp S S | BSApp S S S | BSApp2 S S S S
  deriving (Read, Show)

qs :: Q a -> S
qs QRoot = SRoot
qs (QNice x) = SNice (mkDumDyn x)
qs (QNamed name _) = SNamed name
qs (QApp qf qx) = SApp (qs qf) (qs qx)
qs (BApp qf qr qx) = BSApp (qs qf) (qs qr) (qs qx)
qs (BApp2 qf qr qx qy) = BSApp2 (qs qf) (qs qr) (qs qx) (qs qy)

data Ding = forall a. Typeable a => Ding a Dynamic (Ding -> String)
mkding :: Typeable a => a -> Ding
mkding x = Ding x (toDyn x) stuff
stuff :: Ding -> String
stuff (Ding x' (Dynamic trx xx) _) =
  let qw = typeRep @(Q W)
      qw' = App (typeRep @Q) (typeRep @W)
      same = eqTypeRep qw qw'
   in show ("asdf", same)
  -- All works
  -- let q = case splitApps (typeRep @Q) of (tycon', strs) -> tycon'
  --     App qq (Fun qfa qfr) = typeRep @(Q (W -> Int))
  --     App qq' _ = typeRep @(Q W)
  --     yeah0 = eqTypeRep qfa (typeRep @W)
  --     yeah1 = eqTypeRep qfa (typeRep @Int)
  --     yeah2 = eqTypeRep qq qq'
  --  in show ("heh", yeah0, yeah1, yeah2)

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

-- aa :: Q (W -> Int)
-- root :: Q W
-- QApp :: Q (a -> b) -> Q a -> Q b

gaff :: Dynamic -> Dynamic -> String
gaff (Dynamic atr@(App qq (Fun qfatr qfbtr)) qfab) (Dynamic atr'@(App qq' qatr) qa)
-- gaff (Dynamic atr@(App qq (App (App arrow qfatr) qfbtr)) qfab) (Dynamic atr'@(App qq' qatr) qa)
  | Just HRefl <- qq `eqTypeRep` qq'
  , Just HRefl <- qfatr `eqTypeRep` qatr
  -- = Dynamic (App qq qfbtr) (QApp qfab qa)
  =
  let debug = show ("deeb", withTypeable atr (typeOf qfab),
                    withTypeable atr' (typeOf qa),
                    qatr', qatr'', qfabtr'', qfabtr''', qfbtr, ack0,
                    qfbtr `eqTypeRep` typeRep @Int, wtoi, ack1)
                    -- qbtr)
      qatr' = App qq qatr
      qatr'' = App qq qfatr
      qfabtr'' = App qq (Fun qfatr qfbtr)
      qfabtr''' = App qq (App qq (Fun qfatr qfbtr))
      ack0 = App qq (typeRep @Int)
      wtoi = Fun qfatr qfbtr
      ack1 = App qq wtoi
      -- nope, there's exactly one thing we can't apply Q to, which is b:
      -- ack2 = App qq qfbtr 
      -- ack3 = App (typeRep @(Maybe)) qfbtr 

      -- asdfasdf = App qq (App qq qfbtr)
      -- qbtr' = App qq qfbtr
      -- qbtr = withTypeable atr $ withTypeable atr' $ App qq qfbtr
   in debug
  -- where arrow :: TypeRep ((->) :: TYPE IntRep -> Type -> Type)
  --       arrow = undefined

        -- Dynamic (App qq qfbtr) (QApp qfab qa)
  -- = Dynamic qfbtr (QApp qfab qa)
  -- = Dynamic (App (typeRep @Q) qfbtr) (QApp qfab qa)

  -- let App qq (Fun qfa qfr) = typeRep @(Q (W -> Int))
  --     App qq' (Fun qfa' qfr') = qabtr
  --  in "asdf"
  -- let App qtc (Fun fa fr) = qabtr
  --  in "heyo"

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply' :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply' (Dynamic (Fun ta tr) f) (Dynamic ta' x)
  | Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic tr (f x))
dynApply' _ _
  = Nothing

-- querp :: Dynamic -> Dynamic -> String
-- querp (Dynamic qabtr qab) (Dynamic qatr qa) =
--   let (q, []) = splitApps (typeRep @Q)
--    in case splitApps qabtr of (tycon, strs) -> show (tycon, strs, eqTypeRep tycon q)

-- -- alp = \(_::S) -> toDyn $ QNice $ dumDynToX undefined
-- ooo :: Typeable a => a
-- ooo = undefined
-- -- Can't: even tho ooo is typeable, it's poly so it's not really typeable
-- larr = toDyn ooo

--  :: Q (a->b) -> Q a      -> Maybe (Q b)
qapp :: Dynamic  -> Dynamic -> Maybe Dynamic
qapp (Dynamic qabt@(App q (Fun ta tr)) qf) qat@(Dynamic (App q' ta') qx)
  | Just HRefl <- q `eqTypeRep` q'
  , Just HRefl <- q `eqTypeRep` (typeRep @Q)
  , Just HRefl <- ta `eqTypeRep` ta'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind tr
  = Just (Dynamic (App q tr) (QApp qf qx))
  -- | otherwise =
  -- let qqev = q `eqTypeRep` q'
  --     qQev = q `eqTypeRep` (typeRep @Q)
  --     taev = ta `eqTypeRep` ta'
  --     trev = typeRep @Type `eqTypeRep` typeRepKind tr
  --  in error $ "QApp (" ++ show qabt ++ ") (" ++ show qat ++ ") " ++ show (qqev, qQev, taev, trev)
-- qapp (Dynamic qabt@(App q (Fun ta tr)) qf) qat@(Dynamic (App q' ta') qx) = error "??"
qapp (Dynamic qabt _) (Dynamic qat _) =
   error $ "QApp (" ++ show qabt ++ ") (" ++ show qat ++ ") "

-- BApp :: (Show a, Show b) => Q (a -> b) -> Q (a -> R a -> R b) -> Q a -> Q b
qbapp :: Dynamic  -> Dynamic -> Dynamic -> Maybe Dynamic
qbapp (Dynamic qft@(App qt0 (Fun at0 bt0)) qf)
      (Dynamic qrt@(App qt1 (Fun at1 (Fun (App rt0 at2) y@(App rt1 bt1)))) qr)
      (Dynamic qat@(App qt2 at3) qa)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @Q)
  , Just HRefl <- qt0 `eqTypeRep` qt1
  , Just HRefl <- qt0 `eqTypeRep` qt2
  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- rt0 `eqTypeRep` rt1
  , Just HRefl <- at0 `eqTypeRep` at1
  , Just HRefl <- at0 `eqTypeRep` at2
  , Just HRefl <- at0 `eqTypeRep` at3
  , Just HRefl <- bt0 `eqTypeRep` bt1
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind bt0
  -- , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind y
  -- , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind at2
  -- , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind bt1
  = Just (Dynamic (App qt0 bt0) (BApp qf qr qa))

-- BApp2 :: (Show a, Show b) => Q (a -> b -> c) -> Q (a -> R a -> b -> R b -> R c) -> Q a -> Q b -> Q c
qbapp2 :: Dynamic  -> Dynamic -> Dynamic -> Dynamic -> Maybe Dynamic
qbapp2 (Dynamic qft@(App qt0 (Fun at0 (Fun bt0 ct0))) qf)
       (Dynamic qrt@(App qt1 (Fun at1 (Fun (App rt0 at2) (Fun bt1 (Fun (App rt1 bt2) (App rt2 ct1)))))) qr)
       (Dynamic qat@(App qt2 at3) qa)
       (Dynamic qbt@(App qt3 bt3) qb)
  | Just HRefl <- qt0 `eqTypeRep` (typeRep @Q)
  , Just HRefl <- qt0 `eqTypeRep` qt1
  , Just HRefl <- qt0 `eqTypeRep` qt2
  , Just HRefl <- qt0 `eqTypeRep` qt3

  , Just HRefl <- rt0 `eqTypeRep` (typeRep @R)
  , Just HRefl <- rt0 `eqTypeRep` rt1
  , Just HRefl <- rt0 `eqTypeRep` rt2

  , Just HRefl <- at0 `eqTypeRep` at1
  , Just HRefl <- at0 `eqTypeRep` at2
  , Just HRefl <- at0 `eqTypeRep` at3

  , Just HRefl <- bt0 `eqTypeRep` bt1
  , Just HRefl <- bt0 `eqTypeRep` bt2
  , Just HRefl <- bt0 `eqTypeRep` bt3

  , Just HRefl <- ct0 `eqTypeRep` ct1
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind ct0
  -- , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind y
  -- , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind at2
  -- , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind bt1
  = Just (Dynamic (App qt0 ct0) (BApp2 qf qr qa qb))

sqd :: S -> Dynamic
sqd SRoot = toDyn QRoot
-- sqd (SNice ddyn) = toDyn $ QNice (dumDynToX ddyn)
sqd (SNice ddyn) = dumDynToXShowD ddyn
-- sqd (SNamed name) = toDyn $ QNamed name (reconstitute name)
sqd (SNamed name) = reconstituteShowD name
sqd (SApp sf sx) =
  let dsf = sqd sf
      dsx = sqd sx
   in fromJust $ qapp dsf dsx
sqd (BSApp sf sr sx) =
  let dsf = sqd sf
      dsr = sqd sr
      dsx = sqd sx
   in fromJust $ qbapp dsf dsr dsx
sqd (BSApp2 sf sr sx sy) =
  let dsf = sqd sf
      dsr = sqd sr
      dsx = sqd sx
      dsy = sqd sy
   in fromJust $ qbapp2 dsf dsr dsx dsy
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

sq :: Typeable a => S -> Q a
sq s = fromJustVerbose "sq'" $ fromDynamic $ sqd s

-- -- sq :: (Show a, Read a, Typeable a) => S -> Q a
-- -- Can't do this because then we don't know that the arg to QNice is a show (more
-- -- precisely that we don't know which one, or even more precisely, we can't restrict
-- -- this to only be used *at some Show monotype*.
-- sq :: Typeable a => S -> Q a
-- sq SRoot = fromJustVerbose "sq SRoot" $ fromDynamic $ toDyn QRoot
-- sq (SNice ddyn) = dumDynToXShow ddyn -- QNice (dumDynToX ddyn)
-- sq (SNamed name) = reconstituteShow name -- QNamed name (reconstitute name)
-- -- sq (SApp sf sx) = QApp (sq sf) (sq sx)
-- sq _ = error "sq"

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

-- Minimal example of how we can't treat a (Show a => a) as a Show
-- returnsShow :: Show a => Int -> a
-- returnsShow = undefined
-- data Thing a where
--   ShowThing :: Show a => a -> Thing a
-- foo = ShowThing (returnsShow undefined)

sfaa = qs faa
sfaa' = qs inced
dqfaa = sqd sfaa
qfaa = sq sfaa :: Q Int
-- qfaa = sq' sfaa :: Q Int
dqfaa' = sqd sfaa'
qfaa' = sq sfaa' :: Q Int

logMain = do
  msp $ rd w baa
  msp $ wr w baa (QNice (140::Int))
  msp $ wr w baa (QNice (140::Int))
  msp $ rd w binced
  msp $ wr w binced (QNice (140::Int))
  msp $ rd w bplussed
  msp $ wr w bplussed (QNice (340::Int))
  msp "sss"
  let splussed = qs bplussed
      ssplussed = show splussed
      rsplussed = read ssplussed :: S
      bplussed' = sq rsplussed :: Q Int
  msp $ bplussed
  msp $ splussed
  msp $ ssplussed
  msp $ rsplussed
  msp $ bplussed'

  -- -- Works
  -- msp $ rd w qaa
  -- msp $ wr w qaa (QNice (130 :: Int))
  -- msp $ rd w qaai
  -- msp $ wr w qaai (QNice (130 :: Int))

  -- -- works
  -- msp sfaa
  -- msp dqfaa
  -- msp qfaa
  -- msp $ rd w qfaa
  -- msp sfaa'
  -- msp dqfaa'
  -- msp qfaa'
  -- msp $ rd w qfaa'

  -- works
  -- -- msp $ querp (toDyn vaa) (toDyn QRoot)
  -- msp $ doding foo
  -- msp $ gaff (toDyn vaa) (toDyn root)

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
