module Case
( match ) where

import Env
import Util
import Val

-- TODO Doesn't this exist already?
data Match = Match Env | Failure
  deriving Show

instance Semigroup Match where
  _ <> Failure = Failure
  Failure <> _ = Failure
  Match e0 <> Match e1 = Match (e0 <> e1)

instance Monoid Match where
  mempty = Match newEnv

match :: Val -> [(Val, Code)] -> Maybe (Env, Code)
-- TODO do this with fold or <> or whatever
match _ [] = Nothing
match x ((pat, body) : xs) =
  case match1 pat x of
    Match env -> Just (env, body)
    Failure -> Nothing

match1 :: Val -> Val -> Match
match1 (Val _ uva) (Val _ uvb) = umatch1 uva uvb

match1List :: [Val] -> [Val] -> Match
match1List vsa vsb = foldl (<>) mempty (zipWith match1 vsa vsb)

umatch1 :: UVal -> UVal -> Match
umatch1 pat x = m pat x
  where
    m :: UVal -> UVal -> Match
    -- TODO stripping the type out here
    m (PatVar v) x = Match $ startEnv v (Val DK x)
    m (VI a) (VI b) = checkEq a b
    m (VB a) (VB b) = checkEq a b
    m (VS a) (VS b) = checkEq a b
    m (Cton na argsa) (Cton nb argsb) = checkEq na nb <> match1List argsa argsb
    checkEq a b = if a == b then mempty else Failure
