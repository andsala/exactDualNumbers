{-  To do:
- fix the error message concerning the hidden module  Math.NumberTheory.Logarithms
- to add the other analytic functions defined through Taylor series
- to make the Taylor series generate convergent  sequences of intervals
- complete the Duals definitions

Possible improvements:
- extensively used the class mechanisms
  - make CReal instances of Order, Floating
  - define immediately Intervals, DualIntevals, CReals, Duals,
    paramentric on some basic types
- start with an efficient library representation of the interals

-}


module ExactReal
  where
import           CReal
import           FixedPoint
import           Interval

-----------------
-- Taylor series
----------------

powerSeries x = powerSeriesAux x 1
  where powerSeriesAux x y = y : powerSeriesAux x (y*x)

factorialSeries = factorialSeriesAux 0 1
  where factorialSeriesAux n nf = nf : factorialSeriesAux (n+1) (nf * (n+1))

seriesFromFunction f = map f [0..]

taylorSeries f x = zipWith divIN (zipWith (*) (seriesFromFunction f) (powerSeries x)) factorialSeries

addSeries (x:xs) = addSeriesAux x xs where
  addSeriesAux y (x:xs) = y : addSeriesAux (y+x) xs

functionFromFH f h (R r) =  R (\n -> (addSeries (taylorSeries f (r n))) !! n +
                               (h n (meetI 0 (r n)) * ((powerSeries (r n))!! (n+1)) `divIN` (factorialSeries !! (n+1))))


------------------------
-- Alternative incomplete definition of Taylor series
-------------------------

fact n = product [1 .. n]

power 0 x = 1
power n x = power (n-1) x

taylorI f x = taylorIAux f x 1 0 where
  taylorIAux f x xn n = (f n * xn) `divIN` fact n :
    taylorIAux f x (x * xn) (n+1)
-- build the Taylor series f(0), ...,  f(n)/n! x^n

functionFromFGG f g (R r) =  R (\n -> (addSeries (taylorI f (r n))) !! n)
--  +  (g (meetI 0 (r n)) * power n (r n)) `divIN` (fact n))

--------------------------
-- Some analytic functions
--------------------------

fCos n = case  n `rem` 4 of
           0 -> 1
           2 -> -1
           _ -> 0

hCos _ x = x - x

cosR = functionFromFH fCos hCos

-------------
-- some tests
-------------

newHalf = integrationR id
fastNewHalf = fastIntegrationR id

testFunction x = (1 + projR x) / 3
halfFixed = fixedPoint testFunction

one = cosR 0
almostMinusOne = cosR 3
