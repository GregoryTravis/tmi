module Lens
( field
) where

import Ty
import V

field :: V w r -> String -> (r -> f) -> (r -> f -> r) -> V w f
field rec name f r = VBiSeal (BiApp (vbi name f r') rec)
  where r' w wr = mkR ir
          where ir f = write wr (r w f)
