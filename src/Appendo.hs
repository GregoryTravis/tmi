module Appendo where

appendo :: (Eq a, Show a) => V (Appendo a) -> V a -> TMI WW ()
appendo vapp va =
  let vas = appendo_1 <$$> vapp
   in vas <--- appendV <**> vas <$$> (consV <**> va <$$> (VCheckConst "appendo" []))
