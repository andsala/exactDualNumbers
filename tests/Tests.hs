module Main where

import qualified ExactRealTests   (tests)
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Tests" [ExactRealTests.tests]

main :: IO ()
main = do
    defaultMain tests
