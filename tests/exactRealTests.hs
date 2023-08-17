module ExactRealTests where

import           Test.Tasty
import           Test.Tasty.HUnit

testBase :: TestTree
testBase = testCase "Example test case" $ 2 + 1 @?= 3

tests :: TestTree
tests = testGroup "ExactReal" [testBase]
