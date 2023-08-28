module VPieces
( vPiecesMain ) where

import Narr
import Pers
import TS
import Util

vPiecesMain = do
  let foo :: Int
      foo = readPers $ PApp (TLB "plus1") (Direct (12::Int))
  msp foo
  msp "hey vpieces"
