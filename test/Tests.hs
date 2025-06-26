module Main where

import DisplayTests
import LamTests

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "both" [displayTests, lamTests])
