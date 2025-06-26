module TestUtil
( (~?=) ) where

import Test.Tasty.HUnit

infix 1 ~?=
a ~?= e = testCase "" $ a @?= e
