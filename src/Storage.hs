{-# Language GADTs, ScopedTypeVariables #-}

module Storage
( Reconstitutor
, qs
, sq ) where 

import Data.Dynamic
import Data.Maybe (fromJust)
import Data.Proxy

import Dyn
import Ty
import Util
import V
import Veq

type Reconstitutor = String -> Dynamic

---- Show, Read. First is shewn, second is type
data DumDyn = DumDyn String String deriving (Eq, Read, Show)
mkDumDyn :: (Show a, Read a, Typeable a) => a -> DumDyn
mkDumDyn x = DumDyn (show x) (show (toDyn x))

dumDynToXShowD :: forall w. Typeable w => Proxy w -> DumDyn -> Dynamic
dumDynToXShowD _ (DumDyn s ts) = dyn
  -- where dyn = case ts of "<<Int>>" -> toDyn $ (let v = ((VNice (read s :: Int)) :: V w Int) in v)
  where dyn = case ts of "<<Int>>" -> toDyn $ ((VNice (read s :: Int)) :: V w Int)
                         "<<String>>" -> toDyn $ ((VNice (read s :: String)) :: V w String)
                         _ -> error $ "dumDynToX " ++ ts

data S = SRoot | SNice DumDyn | SNamed String | SVBiSeal BS
  deriving (Eq, Read, Show)
data BS = BSBi S S | BSBiApp BS S
  deriving (Eq, Read, Show)

qs :: V w a -> S
qs VRoot = SRoot
qs (VNice x) = SNice (mkDumDyn x)
qs (VNamed name _) = SNamed name
qs (VBiSeal bi) = SVBiSeal (bs bi)

bs :: Bi w f r -> BS
bs (Bi f r) = BSBi (qs f) (qs r)
bs (BiApp bi q) = BSBiApp (bs bi) (qs q)

sqd :: forall w. Typeable w => Proxy w -> Reconstitutor -> S -> Dynamic
sqd w recon SRoot = toDyn (VRoot :: V w w)
sqd w recon (SNice ddyn) = dumDynToXShowD w ddyn
sqd _ recon (SNamed name) = recon name
sqd w recon (SVBiSeal bis) =
  let dbs = bqd w recon bis
   in fromJust $ qbiseal dbs

-- name should be bsbd
bqd :: Typeable w => Proxy w -> Reconstitutor -> BS -> Dynamic
bqd w recon (BSBi sf sr) =
  let dsf = sqd w recon sf
      dsr = sqd w recon sr
   in fromJust $ bi dsf dsr
bqd w recon (BSBiApp bs s) =
  let dbs = bqd w recon bs
      ds = sqd w recon s
   in fromJust $ bsbiapp dbs ds

sq :: forall a w. (Typeable a, Typeable w) => Reconstitutor -> S -> V w a
sq recon s =
  let pw = Proxy :: Proxy w
   in fromJustVerbose ("sq'", s) $ fromDynamic $ eeesp "heh" $ sqd pw recon s
