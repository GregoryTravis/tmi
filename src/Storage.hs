{-# Language GADTs #-}

module Storage
( Reconstitutor
, qs
, sq ) where 

import Data.Dynamic
import Data.Maybe (fromJust)

import Dyn
import V
import Ty
import Util

type Reconstitutor = String -> Dynamic

---- Show, Read. First is shewn, second is type
data DumDyn = DumDyn String String deriving (Read, Show)
mkDumDyn :: (Show a, Read a, Typeable a) => a -> DumDyn
mkDumDyn x = DumDyn (show x) (show (toDyn x))

dumDynToXShowD :: DumDyn -> Dynamic
dumDynToXShowD (DumDyn s ts) = dyn
  where dyn = case ts of "<<Int>>" -> toDyn $ VNice (read s :: Int)
                         "<<String>>" -> toDyn $ VNice (read s :: String)
                         _ -> error $ "dumDynToX " ++ ts

data S = SRoot | SNice DumDyn | SNamed String | SVBiSeal BS
  deriving (Read, Show)
data BS = BSBi S S | BSBiApp BS S
  deriving (Read, Show)

qs :: V a -> S
qs VRoot = SRoot
qs (VNice x) = SNice (mkDumDyn x)
qs (VNamed name _) = SNamed name
qs (VBiSeal bi) = SVBiSeal (bs bi)

bs :: Bi f r -> BS
bs (Bi f r) = BSBi (qs f) (qs r)
bs (BiApp bi q) = BSBiApp (bs bi) (qs q)

sqd :: Reconstitutor -> S -> Dynamic
sqd recon SRoot = toDyn VRoot
sqd recon (SNice ddyn) = dumDynToXShowD ddyn
sqd recon (SNamed name) = recon name
sqd recon (SVBiSeal bis) =
  let dbs = bqd recon bis
   in fromJust $ qbiseal dbs

-- name should be bsbd
bqd :: Reconstitutor -> BS -> Dynamic
bqd recon (BSBi sf sr) =
  let dsf = sqd recon sf
      dsr = sqd recon sr
   in fromJust $ bi dsf dsr
bqd recon (BSBiApp bs s) =
  let dbs = bqd recon bs
      ds = sqd recon s
   in fromJust $ bsbiapp dbs ds

sq :: Typeable a => Reconstitutor -> S -> V a
sq recon s = fromJustVerbose "sq'" $ fromDynamic $ sqd recon s
