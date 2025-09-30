module Env
( newEnv
, startEnv
, extend
, combineNoClash
, combineManyNoClash
, elookup ) where

import qualified Data.Map.Strict as M
import Control.Applicative ((<|>))

import Adt
import History
import Util
import Val

newEnv :: Env
newEnv = EmptyLayer

startEnv :: Ident -> Val -> Env
startEnv id x = extend EmptyLayer id x

extend :: Env -> Ident -> Val -> Env
extend EmptyLayer id x = MapLayer (M.insert id x M.empty)
extend env id x = Layers (MapLayer (M.insert id x M.empty)) env

-- Search order is left-to-right.

instance Semigroup Env where
  (<>) = Layers

instance Monoid Env where
  mempty = EmptyLayer

-- Error if there is a name conflict.
combineNoClash :: Env -> Env -> Env
combineNoClash (MapLayer m0) (MapLayer m1) = MapLayer $ M.unionWithKey noClash m0 m1
  where noClash k _ _ = error $ "Env clash on " ++ k
combineNoClash EmptyLayer x = x
combineNoClash x EmptyLayer = x
combineNoClash x y = error $ "combineNoClash is only for maplayers: " ++ show x ++ " " ++ show y

combineManyNoClash :: [Env] -> Env
combineManyNoClash envs = foldl combineNoClash EmptyLayer envs

elookup :: History -> Env -> Ident -> Maybe Val
elookup _ (MapLayer map) x = M.lookup x map
elookup h (Layers e0 e1) x = elookup h e0 x <|> elookup h e1 x
elookup h GlobalLayer x =
  let root = latest h
      code = ctonRecLookupMust root "code"
      values = ctonRecLookupMust code "values"
   in ctonRecLookup values x
elookup _ EmptyLayer x = Nothing
