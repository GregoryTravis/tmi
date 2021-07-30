module Appendo where

import Lib
import State
import V

appendo :: (Eq a, Show a) => V (Appendo a) -> V a -> TMI w ()
appendo vapp va =
  let vas = appendo_1 <$$> vapp
   in vas <--- appendV <**> vas <$$> (consV <**> va <$$> VCheckConst "appendo" [])

newtype Appendo a = Appendo [a] deriving Show
appendo_1 :: V (R (Appendo a) -> R [a])
appendo_1 = VConst "appendo_1" __app
  where __app (R ap rap) = R as ras
          where as = case ap of Appendo as -> as
                ras = Receiver "appendo_1" $ \newAs ->
                  rap <-- Appendo newAs
