module Main where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import CaseTests
import EnvTests
import ValTests

main :: IO ()
main = defaultMain (testGroup "all" [valTests, envTests, caseTests])
