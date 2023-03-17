{-# Language GADTs, RankNTypes, ScopedTypeVariables #-}

module Storage
( Reconstitutor
, qs
, unqs
, S
, shower
, readsPrecer
) where 

import Data.Dynamic
import Data.Maybe (fromJust)
import Data.Proxy
-- import Data.Typeable (typeOf)
import Unsafe.Coerce

import Ty hiding (S)
import Util
import V
import Veq

type Reconstitutor = forall a. String -> a

-- ---- Show, Read. First is shewn, second is type
-- data DumDyn = DumDyn String String deriving (Eq, Read, Show)
-- mkDumDyn :: (Show a, Read a, Typeable a) => a -> DumDyn
-- mkDumDyn x = DumDyn (show x) (show (toDyn x))

-- dumDynToXShowD :: forall w. Typeable w => Proxy w -> DumDyn -> Dynamic
-- dumDynToXShowD _ (DumDyn s ts) = dyn
--   -- where dyn = case ts of "<<Int>>" -> toDyn $ (let v = ((VNice (read s :: Int)) :: V w Int) in v)
--   where dyn = case ts of "<<Int>>" -> toDyn $ ((VNice (read s :: Int)) :: V w Int)
--                          "<<String>>" -> toDyn $ ((VNice (read s :: String)) :: V w String)
--                          _ -> error $ "dumDynToX " ++ ts

  -- let baaa = BiApp (unsafeCoerce (recon "aa")) VRoot
  --     slaa = VBiSeal baaa
  --     babb = BiApp (unsafeCoerce (recon "bb")) VRoot
  --     slbb = VBiSeal babb
  --     pl = VBiSeal (BiApp (BiApp (unsafeCoerce (recon "bplus")) slaa) slbb)

dumToShow :: (Eq a, Typeable a) => String -> String -> a
dumToShow s "<<Int>>" = unsafeCoerce (read s :: Int)
dumToShow s "<<String>>" = unsafeCoerce (read s :: String) -- TODO just return s
-- dumToShow s "<<Retval>>" = unsafeCoerce $ eeesp ("umm", s) (read s :: Retval) -- TODO just return s
dumToShow s typeS = error $ "dumToShow?? " ++ s ++ " " ++ typeS

-- dumToShowVNice :: (Eq a, Show a, Read a, Typeable a) => String -> String -> V w a
-- dumToShowVNice s "<<Int>>" = unsafeCoerce $ VNice (read s :: Int)

data S = SRoot | SNice String String | SNamed String | SVBiSeal BS | SVFreeze Int S
  deriving (Eq, Read, Show)
data BS = BSBi S S | BSBiApp BS S
  deriving (Eq, Read, Show)

qs :: V w a -> S
qs VRoot = SRoot
qs (VNice x) = SNice (show x) (show (toDyn x))
qs (VNamed name _) = SNamed name
qs (VBiSeal bi) = SVBiSeal (bs bi)
qs (VFreeze n v) = SVFreeze n (qs v)

bs :: Bi w f r -> BS
bs (Bi f r) = BSBi (qs f) (qs r)
bs (BiApp bi q) = BSBiApp (bs bi) (qs q)

unqs :: Reconstitutor -> S -> V w a
unqs recon SRoot = unsafeCoerce VRoot

-- unqs recon (SNice shown typeS) = VNice $ dumToShow shown typeS
-- unqs recon (SNice shown typeS) =
--   dumToShowVNice shown typeS :: (Show c, Eq c, Read c, Typeable c) => V w c
-- should be t.o.
-- unqs recon (SNice shown typeS) = dumToShowVNice shown typeS
-- unqs recon (SNice shown typeS) = error "VNice"
unqs recon (SNice shown "<<Int>>") = unsafeCoerce $ VNice (read shown :: Int)
unqs recon (SNice shown "<<String>>") = unsafeCoerce $ VNice (read shown :: String)
unqs recon (SNice shown "<<[Char]>>") = unsafeCoerce $ VNice (read shown :: String)
-- unqs recon (SNice shown "<<Retval>>") = unsafeCoerce $ VNice (read shown :: Retval)
unqs recon (SNice shown typeS) = error $ "unqs type?? " ++ shown ++ " " ++ typeS

unqs recon (SNamed name) = recon name
unqs recon (SVBiSeal bis) = VBiSeal (unbs recon bis)

unbs :: Reconstitutor -> BS -> Bi w f r
unbs recon (BSBi sf sv) = Bi (unqs recon sf) (unqs recon sv)
unbs recon (BSBiApp bs sv) = BiApp (unbs recon bs) (unqs recon sv)

shower :: V w a -> String
shower v = show (qs v)

readsPrecer :: Reconstitutor -> Int -> ReadS (V w a)
readsPrecer recon i s =
  let parses = readsPrec i s
    in map (\(s, str) -> (unqs recon s, str)) parses
-- readsPrecer :: Reconstitutor -> String -> V w a
-- readsPrecer recon = (unqs recon) . read
