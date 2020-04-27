{-# LANGUAGE TypeFamilies #-}

module TmiPrelude
( Umm
, (TFDV.<--)
, (TFDV.<-+)
, (!!)
, TFDV.DString(..)
, ints
, strings
, encoder
) where

import Prelude ()
import qualified Prelude as P

import qualified TypeFamilyDV as TFDV
import Util

type Umm a = TFDV.TMI TFDV.W a

(!!) = (TFDV.!!-)
encoder = TFDV.encoderF
strings :: TFDV.V dli ~ [P.String] => TFDV.F (TFDV.DWStrings dli) dli
strings = TFDV.stringsL
ints :: TFDV.V dli ~ [P.Int] => TFDV.F (TFDV.DWInts dli) dli
ints = TFDV.intsL
