module Lens

( mkFielder ) where

mkFielder :: String -> (r -> a) -> (r -> a -> r) -> V (R r -> R a)
mkFielder s fieldFor fieldRev = VConst s __acc
  where __acc (R r rr) = (R a ra)
          where a = fieldFor r
                ra = Receiver s $ \newA ->
                  rr <-- fieldRev r newA

