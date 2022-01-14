module TestUtil where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)

import Core
import Monad

infix 1 ~?=
a ~?= e = testCase "" $ a @?= e

testBlef :: Blef w a -> a -> testTree
testBlef blef expected = M.do
  
testCase "length" $ do
        ls <- lsIO
        length ls @?= 2
