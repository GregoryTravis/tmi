{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Exp (expDemo) where

import Control.Applicative
import Control.Monad.State
import qualified Data.CaseInsensitive as CI
import Data.Function
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Debug.Trace as TR
import Network.HTTP.Types.Status (ok200, found302)
import Network.URI.Encode as ENC
import System.Directory (copyFile)
import System.IO
import Text.Pretty.Simple (pShow)
import Util

{-
data Val a = Val a
data Fun a b = Fun (a -> b)
data App f v = App f v

class FunLike a
instance FunLike (Fun a b)
instance FunLike b => FunLike (App (Fun a b) (Val a))

class Exp a
instance Exp (Val a)
instance (Exp a, Exp b) => Exp (Fun a b)
instance Exp (App f v)

app :: (FunLike f, Exp v) => f -> v -> App f v
app = undefined

class FunLike f => Apply f a b | f a -> b where
  apply :: f -> a -> b

-- http://okmij.org/ftp/Haskell/HList-ext.pdf
instance (FunLike f, Exp a) => Apply (

--class Eval e v -- | e -> v
  --where eval :: e -> v

--instance Exp e => Eval e v where
  --eval (Val x) = x
-}

data Val a = Val a
data App f x = App f x

--class Exp a
--instance Exp (Val a)

class Exp e a | e -> a where
  eval :: e -> a

instance Exp (Val a) a where
  eval (Val a) = a
instance (Exp f (a -> b), Exp x a) => Exp (App f x) b where
 eval (App f x) = (eval f) (eval x)

val x = Val x
liftF f = App (val f)
liftF2 f x y = App (App (val f) x) y

(+.) = liftF2 (+)

expDemo = do
  let foo = Val 3
  --aa <- makeStableName foo
  --bb <- makeStableName foo
  --cc <- makeStableName foo
  --msp $ hashStableName aa
  --msp $ hashStableName bb
  --msp $ hashStableName cc
  msp $ eval (App (Val (+2)) (Val 10))
  msp $ eval (App (App (Val (+)) (Val 2)) (Val 10))
  msp $ eval (App (Val (++ "asdf")) (Val "zxcv"))
  msp $ eval (App (App (Val (++)) (App (Val show) (Val 2))) (Val "asdf"))
  msp $ eval $ (val 3) +. (val 12)
  --msp $ eval (App (Val (+2)) (Val "a"))
  msp "hi"

--data Val a b = Val (a -> b) a
--data App a b c d = App (Val a b) (Val c d)
--appToVal :: App a b c d -> 

{-
data Val a = Val a
  deriving Show
data Fun a b = Fun String (a -> b)
data App a b = App a b
  deriving Show

instance Show (Fun a b) where
  show (Fun s f) = "<Fun " ++ s ++ ">"

forwards :: App a b -> IO ()
forwards (App f v@(App a b)) = do
  msp f
  forwards v
forwards (App f (Val v)) = do
  msp f
  msp v

main = do
  msp $ App 2 3
  msp $ App (App 2 3) (App 4 5)
  msp $ App (Fun "a" (+2)) (App (Fun "b" (+100)) (App (Fun "c" (+10000)) (Val 12)))
  msp "hi"
-}

{-
data Ofun a b = Ofun (a -> b)
instance Show (Ofun a b) where
  show _ = "<Fun>"

data Boo a b = Val b | App (Boo () (a -> b)) (Boo () a)
--instance (Show a, Show b) => Show (Boo a b) where
  --show (Val b) = show b
  --show (App f x) = "(" ++ "<Fun>" ++ " " ++ (show x) ++ ")"
instance Show (Boo a b) where
  show (Val v) = "val"
  show (App f x) = "(" ++ (show f) ++ " " ++ (show x) ++ ")"
mkFun :: (a -> b) -> Boo () (a -> b)
mkFun f = Val f
mkVal :: a -> Boo () a
mkVal x = Val x
--mkApp :: (a -> b) -> a -> Boo a b
--mkApp f x = App (Val f) (Val x)
--mkApp (Val f) (Val x) = App (Val f) (Val x)
mkApp :: Boo () (a -> b) -> Boo () a -> Boo a b
mkApp f x = App f x

eval :: Boo a b -> b
eval (Val b) = b
eval (App f x) = (eval f) (eval x)

main = do
  let f = mkFun (+2)
      x = mkVal 3
      app = mkApp f x
      res = eval app
      res' = eval (mkApp (mkFun (+2)) (mkVal 3))
      --res2 = eval (mkApp (mkFun (+100)) app)
  msp f
  msp x
  msp app
  msp res
  --msp res2
  msp "hi"
-}
