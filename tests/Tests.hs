module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified ExactRealTests (tests)


tests :: TestTree
tests = testGroup "Tests" [ExactRealTests.tests]

main :: IO ()
main = do
    defaultMain tests
