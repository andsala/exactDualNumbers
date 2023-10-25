module CDual
  where
import           DualInterval

------------------------------
-- Computable Duals
------------------------
newtype CDual = D (Int -> DualInterval)
unD (D x) = x

embedValueD i = D (\n -> i)
embedUnaryD f r = D (\n -> f (unD r n))
embedBinaryD f r1 r2 = D (\n -> f (unD r1 n) (unD r2 n))

toD i = embedValueD (toDI i)
unitD = embedValueD unitDI
oneD  = toD 1
halfD = embedValueD  halfDI
addD  = embedBinaryD addDI
negD  = embedUnaryD  negDI
multD = embedBinaryD multDI
invD  = embedUnaryD  invDI
maxD  = embedBinaryD maxDI
divDN r d n = divDIN (r n) d
divD2 = embedUnaryD  divDI2



