module Main where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import ValTests

main :: IO ()
main = defaultMain (testGroup "all" [lamTests])
