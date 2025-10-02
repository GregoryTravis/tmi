module Main where

import Test.Tasty (defaultMain, testGroup, localOption, TestTree)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import CaseTests
import EnvTests
import StdLibTests

main :: IO ()
main = defaultMain (testGroup "all" [envTests, caseTests, stdLibTests])
