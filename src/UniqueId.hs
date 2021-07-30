module UniqueId
( UniqueId(..) ) where

newtype UniqueId = UniqueId (Int, Int) deriving (Eq, Show)
