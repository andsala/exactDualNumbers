module CReal
  where
import           Interval


newtype CReal = R (Int -> Interval)
unR (R x) = x
-- in the case, but not in the present implementation,
-- one implements  real numbers as Cauchy sequences, r should be such
-- if unR r n = I man gap exp then  2^-n > gap * 2^-exp

embedValue i        = R (\n -> i)
embedUnary  f r     = R (\n -> (roundI (f (unR r n))))
embedBinary f r1 r2 = R (\n -> (roundI (f (unR r1 n) (unR r2 n))))

toR i = embedValue (toI i)
unitR = embedValue unitI
oneR  = toR 1
halrR = embedValue halfI
addR  = embedBinary addI
negR  = embedUnary negI
multR = embedBinary multI
invR  = embedUnary invI
maxR  = embedBinary maxI
minR  = embedBinary minI
projR = embedUnary projI
meetR = embedBinary meetI
roundR = embedUnary roundI

--  divRN r d = R (\n -> divIN (unR r n) d )
divRN r d = embedUnary (\ i -> divIN i d ) r

divR2 = embedUnary divI2

------------------------
-- Class Instances
--------------------

instance Num CReal where
  (+) = addR
  (*) = multR
  a - b = a + negR b
  fromInteger = toR
  abs x = maxR x (-x)
  signum _ = error "No signum!"

instance Fractional CReal where
  x / y = x * invR y
  fromRational _ = error "fromRational not yet implemented."

instance Show CReal where
  show (R x) = (show (x 0)) ++ (show (x 1)) ++ (show (x 4)) ++ (show (x 16))  ++ (show (x 64))



-------------------------------------------------
-- Functionals: integration, supremum, fixed-point
-------------------------------------------------

integrationR f = R (\n -> integrationAux f (n, n))
  where
    integrationAux f (0, n) = unR (f unitR) n
    integrationAux f (m, n) = divI2 (integrationAux (\x -> f (divR2 x)) (m-1, n)) + divI2 (integrationAux (\x -> f (divR2 (x + 1))) (m-1, n))

supremumR f = R (\n -> supremumAux f (n, n))
  where
    supremumAux f (0, n) = unR (f unitR) n
    supremumAux f (m, n) = supremumAux (\x -> f (divR2 x)) (m-1, n) `maxI` supremumAux (\x -> f (divR2 (x + 1))) (m-1, n)

fastIntegrationR f = R(\n -> addList (map (\i -> unR (f (R (\m -> i))) n) (splitUnitInterval n)) `multExp2I` (-n-2))
  where
    splitUnitInterval n = [ I i 1 (-n) | i <- [0..2^n-1]]
    addList xs = addListAux xs 0
      where
        addListAux [] acc     = acc
        addListAux (x:xs) acc = seq (x + acc) (addListAux xs (x + acc))

-- integrationD f n = integrationAux f (n, n)
--   where
--     integrationAux f (0, n) = f unitD n
--     integrationAux f (m, n) = divDI2 (integrationAux (f . divD2) (m-1, n)) `addDI`
--                               divDI2 (integrationAux (f . divD2 . (addD oneD)) (m-1, n))

-- newHalfD = integrationD id
-- test = integrationD (\x -> maxD x (addD oneD (negD x)))

