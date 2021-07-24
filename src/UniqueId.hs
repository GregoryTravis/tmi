module UniqueId
( UniqueId(..) ) where

data UniqueId = UniqueId (Int, Int) deriving (Eq, Show)
