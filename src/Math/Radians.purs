module Math.Radians where

import Prelude

newtype Radians = Radians Number

instance eqRadians :: Eq Radians where
	eq (Radians x) (Radians y) = x == y

instance ordRadians :: Ord Radians where
	compare (Radians x) (Radians y) = compare x y

instance semiringRadians :: Semiring Radians where
	add (Radians x) (Radians y) = Radians (x + y)
	mul (Radians x) (Radians y) = Radians (x * y)
	zero = Radians 0.0
	one = Radians 1.0

instance ringRadians :: Ring Radians where
	sub (Radians x) (Radians y) = Radians (x - y)

instance moduloSemiringRadians :: ModuloSemiring Radians where
	div (Radians x) (Radians y) = Radians (x / y)
	mod _ _ = Radians 0.0

instance divisionRingRadians :: DivisionRing Radians

instance numRadians :: Num Radians

instance showRadians :: Show Radians where
	show (Radians n) = "(Radians " ++ show n ++ ")"
