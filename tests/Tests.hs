module Main where

import qualified IntervalTests    (tests)
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Tests" [IntervalTests.tests]

main :: IO ()
main = do
    defaultMain tests
