module DualInterval
  where
import           Interval

---------------------------------------------------
-- Partial Duals
---------------------------------------------------

-- a data type for partial duals,
-- defined as a pair of intervals
data DualInterval = !Interval :+& !Interval
  deriving (Eq, Show)

-- redefinition of function on dual intervals
toDI i = toI 1 :+& toI 0

multExp2DI (xa :+& xi) n =  multExp2I xa n :+&  multExp2I xi n

oneDI  = toDI 1
halfDI = oneDI `multExp2DI` 1

unitDI = unitI :+& 0

addDI (xa :+& xi) (ya :+& yi) = (xa + ya) :+& (xi + yi)

negDI (xa :+& xi)  = negI xa :+& negI xi

multDI (xa :+& xi) (ya :+& yi) = multI xa ya :+& addI (multI xa yi) (multI xi ya)

divDIN (xa :+& xi) n = divIN xa n :+& divIN xi n

divDI2 (xa :+& xi)  = multExp2DI (xa :+& xi) (-1)

-----------------
-- the following two definitions need to be completed

invDI (xa :+& xi) = oneDI

maxDI (I m1 g1 e1 :+& xi) (I m2 g2 e2  :+& yi) =
  if (e2 <= e1)
  then
    let m1n = m1 `multExp2` (e1-e2)
        g1n = g1 `multExp2` (e1-e2)
        in if m1n - g1n > m2 + g2
           then (I m1 g1 e1  :+& xi)
           else if  m1n + g1n < m2 - g2
                then (I m2 g2 e2  :+& yi)
                else
                  let ml = (m1n - g1n) `max` (m2 - g2)
                      mu = (m1n + g1n) `max` (m2 + g2)
                      in I ((mu + ml    ) `multExp2` (-1))
                            ((mu - ml + 1) `multExp2` (-1)) e2
                         :+& meetI xi yi
  else maxDI (I m2 g2 e2  :+& yi) (I m1 g1 e1 :+& xi)
