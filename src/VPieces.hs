module VPieces
( vPiecesMain ) where

import Narr
import Pers
import TS
import Util

vPiecesMain = do
  msp $ ((readPers $ PApp (TLB "plus1") (Direct 12)) :: Int)
  let narr = NBind (return 22) (\x -> (NBind (msp x) (\_ -> Done)))
  () <- simulate narr
  msp "hey vpieces"
