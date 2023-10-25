module Interval  where
import           Data.Bits
import           Math.NumberTheory.Logarithms

--------------------------------------------
-- Partial reals represented as
-- generalized dyadic intervals
------------------------------------------

-- | A data type for dyadic intervals
-- the expression (I lowBound gap exponent) represents the interval
-- [lowerBound*2^exponent, (lowerBound+gap)*2^exponent]
data Interval = I
    !Integer -- ^ Low bound
    !Integer -- ^ Gap
    !Int     -- ^ Exponent
  deriving (Eq)


-- | the bottom interval [-infinity,+infinity]
-- is represented by an Interval by an exponent having the
-- maximal value (similarly to IEEE floating point notation)
bottomInterval = I (-1) 2 maxValue
isBottom (I _ _ e) = e == maxValue

-- to avoid overflow errors, we use as maxValue for exponent
-- a value smaller that the maximum value of type Int
maxValue = maxBound `div` 2

-- | an efficient implementation of the function \ a n -> a * 2^n
multExp2 a n = shift a n

-- | Map a integer i to the corresponding interval `[i,i]`
toI i = I i 0 0

-- | The interval [-1, 1]
unitI = I (-1) 2 0
-- | The interval [-1/2, 1/2]
halfI = I (-1) 2 (-1)

-- mulExp2I i n = i * 2^n
multExp2I (I l g e) n = I l g (e+n)

-- | addition on intervals
addI (I l1 g1 e1) (I l2 g2 e2) | e2 < e1   = I (l1 `multExp2` (e1-e2) + l2)
                                               (g1 `multExp2` (e1-e2) + g2) e2
addI (I l1 g1 e1) (I l2 g2 e2) | otherwise = I (l1 + l2 `multExp2` (e2-e1))
                                               (g1 + g2 `multExp2` (e2-e1)) e1

-- | additive inverse on intervals
negI (I l g e) = I (-l-g) g e

-- | sign of an interval
signumI (I 0 0 e) = 0
signumI (I l g e) =
  let ub = max l (l+g)
      lb = min l (l+g)
      s = ub + lb
  in if s == 0 then 1 else I (signum s) 0 0

-- | multiplication on intervals
multI (I l1 g1 e1) (I l2 g2 e2) =
  let { ll = l1*l2; lg1 = l2*g1; lg2 = l1*g2; gg = lg1 + lg2 + g1*g2 }
  in let minI = 0 `min` lg1 `min` lg2 `min` gg
         maxI = 0 `max` lg1 `max` lg2 `max` gg
     in I (ll + minI) (maxI - minI) (e1+e2)

-- | the maxPrecision used for rational, in future works,
-- it should be replace by a parameter depending on the level of computation
maxPrecision = 100

-- | a max number of digit to be used for the gap in a normalised form
maxGBits = 24

-- | division of an interval by a natural number
divIN (I l g e) 1 = I l g e
divIN (I l g e) 2 = I l g (e-1)
divIN (I l g e) n =
  let ln = max 0 (maxGBits + integerLog2 n -
                  (if g == 0 then (- maxPrecision) else integerLog2 g))
  in  I ((l `multExp2` ln) `div` n)
        ((g `multExp2` ln) `div` n +2)
        (e - ln)

-- | division of an interval by 2
divI2 :: Interval -> Interval
divI2 x = divIN x 2

-- multiplicative inverse
invI (I l g e) =
  let den = (l+g)*l
      in if den > 0
         then divIN (I l g e) den
         else error "division by zero"

-- minimum
minI (I l1 g1 e1) (I l2 g2 e2) =
  if e2 <= e1
  then let l1n = l1 `multExp2` (e1-e2)
           g1n = g1 `multExp2` (e1-e2)
       in if l1n + g1n <= l2
          then I l1 g1 e1
          else
            let lb = l1n `min` l2
                ub = (l1n + g1n) `min` (l2 + g2)
            in I lb (ub - lb) e2
  else minI (I l2 g2 e2) (I l1 g1 e1)

-- maximum
maxI x y = negI (minI (negI x) (negI y))

-- convex union of two intervals
meetI (I l1 g1 e1) (I l2 g2 e2) | e2 <= e1 =
  let l1n = l1 `multExp2` (e1-e2)
      g1n = g1 `multExp2` (e1-e2)
      lb = l1n `min` l2
      ub = (l1n + g1n) `max` (l2 + g2)
  in I lb (ub - lb) e2
                                | otherwise =
  meetI (I l2 g2 e2) (I l1 g1 e1)

-- projection on the unit interval
projI (I l g e) =
  if e < 0
  then
    let lb =  (-(1 `multExp2` (-e))) `max` l `min` (1 `multExp2` (-e))
        in I lb
           (0 `max` (g + l - lb) `min` (1 `multExp2` (-e) - lb))
           e
  else
    let lb =  (-1) `max` (l `multExp2` e) `min` 1
        in I lb
           (0 `max` ((g + l) `multExp2` e - lb) `min` (1 - lb))
           0

-- round an interval to avoid too many almost useless digits
roundI (I l g e) =
  let gExtraBits = integerLog2 (g+1) - maxGBits
  in if gExtraBits > 0 then I (l `multExp2` (- gExtraBits))
                              (g `multExp2` (- gExtraBits) + 2)
                              (e + gExtraBits)
     else I l g e

toPair (I l g e) =
  let lb =  fromIntegral l * 2 ** fromIntegral e
      gap = fromIntegral g * 2 ** fromIntegral e
  in (lb, lb + gap)

debug (I l g e) = "I " ++ show l ++ " " ++ show g ++ " " ++ show e

---------------------
-- Class Instances --
---------------------

instance Num Interval where
  (+) = addI
  (*) = multI
  a - b = a + negI b
  fromInteger = toI
  abs x = maxI x (-x)
  signum = signumI

instance Show Interval where
  show x =
    let (l, u) = toPair x
    in '[' : show l ++ ", " ++ show u ++ "]"
