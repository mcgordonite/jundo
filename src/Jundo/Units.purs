-- | Types representing the units of measure used in the application. For example, it should not be possible
-- | to pass a length to a function which expects a time.
module Jundo.Units where

import Prelude
import Data.Time
import Data.Vector


-- | Class for types representing a value in a given unit of measure. The toNumber method should return the value, 
-- | and fromNumber should construct the type from a value.
-- |
-- | Instances should satisfy the following law:
-- | `toNumber (fromNumber x) = x`
class Measure u where
  toNumber :: u -> Number
  fromNumber :: Number -> u

-- | Class which marks a unit (c) as a composite unit created by multiplication of a and b.
-- | The instances of Measure in this module all have Semiring instances for convenience. It is unlikely that
-- | the (*) function will actually be useful, since, for example, multiplying a value with a dimension
-- | by another value with a dimension should give a value with the dimension squared, not the original dimension.
class (Measure a, Measure b, Measure c) <= MulMeasure a b c

-- | Utility to apply a function of two numbers to the numeric values of unit types.
mapMeasure :: forall a b c. (Measure a, Measure b, Measure c) => (Number -> Number -> Number) -> a -> b -> c
mapMeasure f a b = fromNumber $ f (toNumber a) (toNumber b)

-- | Multiply two values with units to create a value in the unit composed from the original units.
mulMeasure :: forall a b c. (Measure a, Measure b, Measure c, MulMeasure a b c) => a -> b -> c
mulMeasure = mapMeasure (*)

-- | Divide two values with units to create a value in the unit composed from the original units.
divMeasure :: forall a b c. (Measure a, Measure b, Measure c, MulMeasure b c a) => a -> b -> c
divMeasure = mapMeasure (/)

-- | Multiply a value in a unit of measure by a scalar.
scaleMeasure :: forall a. (Measure a) => Number -> a -> a
scaleMeasure x u = fromNumber $ x * (toNumber u)

-- | Compare two values in a unit by thier numeric value.
measureEq :: forall u. (Measure u) => u -> u -> Boolean
measureEq a b = (toNumber a) == (toNumber b)

-- | Utility function for printing values in a unit.
measureShow :: forall u. (Measure u) => String -> u -> String
measureShow name x = "(" ++ (show (toNumber x)) ++ " " ++ name ++ ")"

-- | Multiply each value in a vector of values with units by a scalar.
vScaleMeasure :: forall u s. (Measure u) => u -> Vec s Number -> Vec s u
vScaleMeasure n = map (\x -> scaleMeasure x n)


instance measureSeconds :: Measure Seconds where
  toNumber (Seconds x) = x
  fromNumber = Seconds


-- | Type representing a number of radians.
newtype Radians = Radians Number

instance measureRadians :: Measure Radians where
  toNumber (Radians x) = x
  fromNumber = Radians

instance semiringRadians :: Semiring Radians where
  add = mapMeasure (+)
  zero = Radians zero
  mul = mapMeasure (*)
  one = Radians one

instance ringRadians :: Ring Radians where
  sub = mapMeasure (-)

instance moduloSemiringRadians :: ModuloSemiring Radians where
  div = mapMeasure (/)
  mod = mapMeasure mod

instance divisionRingRadians :: DivisionRing Radians
instance numRadians :: Num Radians

instance eqRadians :: Eq Radians where
  eq = measureEq

instance showRadians :: Show Radians where
  show = measureShow "radians"


-- | Type representing a number of metres.
newtype Metres = Metres Number

instance measureMetres :: Measure Metres where
  toNumber (Metres x) = x
  fromNumber = Metres

instance semiringMetres :: Semiring Metres where
  add = mapMeasure (+)
  zero = Metres zero
  mul = mapMeasure (*)
  one = Metres one

instance ringMetres :: Ring Metres where
  sub = mapMeasure (-)

instance moduloSemiringMetres :: ModuloSemiring Metres where
  div = mapMeasure (/)
  mod = mapMeasure mod

instance divisionRingMetres :: DivisionRing Metres
instance numMetres :: Num Metres

instance eqMetres :: Eq Metres where
  eq = measureEq

instance showMetres :: Show Metres where
  show = measureShow "m"


-- | Type representing a number of metres per second.
newtype MetresPerSecond = MetresPerSecond Number

instance measureMetresPerSecond :: Measure MetresPerSecond where
  toNumber (MetresPerSecond x) = x
  fromNumber = MetresPerSecond

instance semiringMetresPerSecond :: Semiring MetresPerSecond where
  add = mapMeasure (+)
  zero = MetresPerSecond zero
  mul = mapMeasure (*)
  one = MetresPerSecond one

instance ringMetresPerSecond :: Ring MetresPerSecond where
  sub = mapMeasure (-)

instance moduloSemiringMetresPerSecond :: ModuloSemiring MetresPerSecond where
  div = mapMeasure (/)
  mod = mapMeasure mod

instance divisionRingMetresPerSecond :: DivisionRing MetresPerSecond
instance numMetresPerSecond :: Num MetresPerSecond

instance eqMetresPerSecond :: Eq MetresPerSecond where
  eq = measureEq

instance showMetresPerSecond :: Show MetresPerSecond where
  show = measureShow "m/s"

instance mulMeasureMSS :: MulMeasure MetresPerSecond Seconds Metres
instance mulMeasureSMS :: MulMeasure Seconds MetresPerSecond Metres


-- | Type representing a number of radians per second.
newtype RadiansPerSecond = RadiansPerSecond Number

instance measureRadiansPerSecond :: Measure RadiansPerSecond where
  toNumber (RadiansPerSecond x) = x
  fromNumber = RadiansPerSecond

instance semiringRadiansPerSecond :: Semiring RadiansPerSecond where
  add = mapMeasure (+)
  zero = RadiansPerSecond zero
  mul = mapMeasure (*)
  one = RadiansPerSecond one

instance ringRadiansPerSecond :: Ring RadiansPerSecond where
  sub = mapMeasure (-)

instance moduloSemiringRadiansPerSecond :: ModuloSemiring RadiansPerSecond where
  div = mapMeasure (/)
  mod = mapMeasure mod

instance divisionRingRadiansPerSecond :: DivisionRing RadiansPerSecond
instance numRadiansPerSecond :: Num RadiansPerSecond

instance eqRadiansPerSecond :: Eq RadiansPerSecond where
  eq = measureEq

instance showRadiansPerSecond :: Show RadiansPerSecond where
  show = measureShow "radians/s"

instance mulMeasureRSS :: MulMeasure RadiansPerSecond Seconds Radians
instance mulMeasureSRS :: MulMeasure Seconds RadiansPerSecond Radians
