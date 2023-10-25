module IntervalTests where

import           Control.Monad    (unless)
import           Interval
import           Test.Tasty
import           Test.Tasty.HUnit

testEq = testGroup "Eq" [
    testCase "(toI 3) == (toI 3)" $ toI 3 == toI 3 @?= True,
    testCase "(toI 3) == (toI 4)" $ toI 3 == toI 4 @?= False]

testBottomInterval = testCase "isBottom (bottomInterval)" $ isBottom bottomInterval @?= True

testNegI = testGroup "negI" [
    testCase "(toI 3)" $ negI (toI 3) @?= I (-3) 0 0,
    testCase "(I 2 4 2)" $ negI (I 2 4 2) @?= I (-6) 4 2]

testShow = testGroup "show" [
    testCase "(toI 3)" $ show (toI 3) @?= "[3.0, 3.0]",
    testCase "(unitI)" $ show unitI @?= "[-1.0, 1.0]",
    testCase "(halfI)" $ show halfI @?= "[-0.5, 0.5]"]

testNumAdd = testGroup "(+)" [
    testCase "(toI 3 + toI 3)" $ toI 3 + toI 3 @?= I 6 0 0,
    testCase "(toI 3 + toI 4)" $ toI 3 + toI 4 @?= I 7 0 0,
    testCase "(toI 4 + toI 3)" $ toI 4 + toI 3 @?= I 7 0 0,
    testCase "(unitI + halfI)" $ unitI + halfI @?= I (-3) 6 (-1),
    testCase "(halfI + unitI)" $ halfI + unitI @?= I (-3) 6 (-1)]

testNumSub = testGroup "(-)" [
    testCase "(toI 3 - toI 3)" $ toI 3 - toI 3 @?= I 0 0 0,
    testCase "(toI 3 - toI 4)" $ toI 3 - toI 4 @?= I (-1) 0 0,
    testCase "(toI 4 - toI 3)" $ toI 4 - toI 3 @?= I 1 0 0]

testNumMul = testGroup "(*)" [
    testCase "3 * 0" $ toI 3 * toI 0 @?= I 0 0 0,
    testCase "0 * 3" $ toI 0 * toI 3 @?= I 0 0 0,
    testCase "3 * 1" $ toI 3 * toI 1 @?= I 3 0 0,
    testCase "1 * 3" $ toI 1 * toI 3 @?= I 3 0 0,
    testCase "[-1,1] * [3,3]" $ I (-1) 2 0 * I 3 0 0 @?= I (-3) 6 0]

testNumFromInteger = testGroup "fromInteger" [
    testCase "0" $ 0 @?= I 0 0 0,
    testCase "3" $ 3 @?= I 3 0 0]

testNumAbs = testGroup "abs" [
    testCase "3" $ abs (I 3 0 0) @?= I 3 0 0,
    testCase "-3" $ abs (I (-3) 0 0) @?= I 3 0 0,
    testCase "[-2,2]" $ abs (I (-2) 4 0) @?= I (-2) 4 0,
    testCase "[-3,1]" $ abs (I (-3) 4 0) @?= I (-1) 4 0,
    testCase "[-1,3]" $ abs (I (-1) 4 0) @?= I (-1) 4 0]

testNumSignum = testGroup "signum" [
    testCase "3" $ signum (I 3 0 0) @?= I 1 0 0,
    testCase "0" $ signum (I 0 0 0) @?= I 0 0 0,
    testCase "-3" $ signum (I (-3) 0 0) @?= I (-1) 0 0,
    testCase "[-3,1]" $ signum (I (-3) 4 0) @?= I (-1) 0 0,
    testCase "[-1,3]" $ signum (I (-1) 4 0) @?= I 1 0 0,
    testCase "[-1,1]" $ signum (I (-1) 2 0) @?= I 1 0 0]

signumLaw x = abs x * signum x @?= x
testNumSignumLaw = testGroup "signum law" [
    testCase "3" $ signumLaw (I 3 0 0),
    testCase "0" $ signumLaw (I 0 0 0),
    testCase "-3" $ signumLaw (I (-3) 0 0),
    testCase "[-3,1]" $ signumLaw (I (-3) 4 0),
    testCase "[-1,3]" $ signumLaw (I (-1) 4 0),
    testCase "[-1,1]" $ signumLaw (I (-1) 2 0)]

testNum = testGroup "Num" [testNumAdd, testNumSub, testNumMul, testNumFromInteger, testNumAbs, testNumSignum, testNumSignumLaw]

testDivN = testGroup "divIN" [
    testCase "[1,1] / 1" $ divIN (I 1 0 0) 1 @?= I 1 0 0,
    testCase "[1,3] / 1" $ divIN (I 1 2 0) 1 @?= I 1 2 0,
    testCase "[2,6] / 1" $ divIN (I 2 4 0) 1 @?= I 2 4 0,
    testCase "[-1,1] / 1" $ divIN (I (-1) 2 0) 1 @?= I (-1) 2 0,

    testCase "[2,2] / 2" $ assertSimilar (0, 0) (I 1 0 0) (divIN (I 2 0 0) 2),
    testCase "[1,3] / 2" $ divIN (I 1 2 0) 2 @?= I 1 2 (-1),
    testCase "[2,6] / 2" $ divIN (I 2 4 0) 2 @?= I 2 4 (-1),
    testCase "[-2,2] / 2" $ assertSimilar (0, 0) (I (-1) 2 0) (divIN (I (-2) 4 0) 2),
    
    -- testCase "[1,1] / 3" $ assertSimilar (0, 0) (I 1 0 0) (divIN (I 1 0 0) 3),
    testCase "[3,3] / 3" $ assertSimilar (0, 0) (I 1 0 0) (divIN (I 3 0 0) 3),
    testCase "[0.5,1.5] / 3" $ assertSimilar (1, 1) (I 1 2 (-1)) (divIN (I 1 2 0) 3),
    testCase "[1,3] / 3" $ assertSimilar (1, 1) (I 2 4 (-1)) (divIN (I 2 4 0) 3),
    testCase "[-3,3] / 3" $ assertSimilar (0, 0.0000005) (I (-1) 2 0) (divIN (I (-3) 6 0) 3)]

tests :: TestTree
tests = testGroup "Interval" [testEq, testBottomInterval, testNegI, testShow, testNum, testDivN]

assertSimilar :: (Floating a, Ord a, Show a) => (a, a) -> Interval -> Interval -> IO ()
assertSimilar (t1, t2) expected actual =
  let (e1, e2) = toPair expected
      (a1, a2) = toPair actual
      ok1 = e1-t1 <= a1 && a1 <= e1+t1
      ok2 = e2-t2 <= a2 && a2 <= e2+t2
      compare = "(" ++ show actual ++ " != " ++ show expected ++ ")"
  in do {
    unless ok1 (assertFailure ("expected lower bound to be between " ++ show (e1-t1) ++ " and " ++ show (e1+t1) ++ "\nbut got: " ++ show a1 ++ " " ++ compare));
    unless ok2 (assertFailure ("expected upper bound to be between " ++ show (e2-t2) ++ " and " ++ show (e2+t2) ++ "\nbut got: " ++ show a2 ++ " " ++ compare))
  }
