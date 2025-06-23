module OptLam
( optLamMain ) where

import Display
import Lst
import Rec
import Rel
import Tuple
import Util
import Value

p = putStr . displayValue

nodes :: Typ
nodes = TAdt "Nodes" [TCtor "Lam" [], TCtor "App" [], TCtor "Share" [], TCtor "Unshare" []]

optLamMain = do
  let l0 = lstFromList [I 10, I 20]
  p l0
  msp "opt lam hi"
