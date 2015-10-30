module Jundo.Units where

import Prelude
import Data.Time
import Data.Vector


-- toNumber (fromNumber x) == x
class Measure u where
  toNumber :: u -> Number
  fromNumber :: Number -> u

class (Measure a, Measure b, Measure c) <= MulMeasure a b c

mapMeasure :: forall a b c. (Measure a, Measure b, Measure c) => (Number -> Number -> Number) -> a -> b -> c
mapMeasure f a b = fromNumber $ f (toNumber a) (toNumber b)

mulMeasure :: forall a b c. (Measure a, Measure b, Measure c, MulMeasure a b c) => a -> b -> c
mulMeasure = mapMeasure (*)

divMeasure :: forall a b c. (Measure a, Measure b, Measure c, MulMeasure b c a) => a -> b -> c
divMeasure = mapMeasure (/)

scaleMeasure :: forall a. (Measure a) => Number -> a -> a
scaleMeasure x u = fromNumber $ x * (toNumber u)

measureEq :: forall u. (Measure u) => u -> u -> Boolean
measureEq a b = (toNumber a) == (toNumber b)

measureShow :: forall u. (Measure u) => String -> u -> String
measureShow name x = "(" ++ (show (toNumber x)) ++ " " ++ name ++ ")"

vScaleMeasure :: forall u s. (Measure u) => u -> Vec s Number -> Vec s u
vScaleMeasure n = map (\x -> scaleMeasure x n)


instance measureSeconds :: Measure Seconds where
  toNumber (Seconds x) = x
  fromNumber = Seconds


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

