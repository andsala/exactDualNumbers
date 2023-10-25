module FixedPoint
  where
import           CReal
import           Interval

class FixedPoint a where
  down       :: a -> a
  fixedPoint :: (a -> a) -> a
  fixedPoint f = f (down (fixedPoint f))

instance FixedPoint CReal where
  down (R x) = R( downf x)
    where
      downf x 0 = bottomInterval
      downf x n = x (n-1)

instance (FixedPoint a) => FixedPoint (a -> b) where
   down f x = f (down x)
