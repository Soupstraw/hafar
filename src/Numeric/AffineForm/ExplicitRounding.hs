-- | ExplicitRounding defines the ExplicitRounding class and
-- instances for some more common numeric types
module Numeric.AffineForm.ExplicitRounding (
                                ExplicitRounding,
                                eps, prev, next,
                                (+/), (+\),
                                (-/), (-\),
                                (*/), (*\)
                                ) where

import Numeric.Interval as IA
import Data.Ratio

-- | The class of numeric values that can be rounded explicitly
class (Ord a, Num a) => ExplicitRounding a where
  -- | Return some number so that all the values that could be rounded to the parameter of this function
  -- would be at most that distance away from that parameter.
  eps :: a -> a
  -- | Returns parameter plus its epsilon
  prev :: a -> a
  -- | Returns parameter minus its epsilon
  next :: a -> a
  -- | Add the two values, rounding the result up
  (+/) :: a -> a -> a
  -- | Add the two values, rounding the result down
  (+\) :: a -> a -> a
  -- | Subtract the two values, rounding the result up
  (-/) :: a -> a -> a
  -- | Subtract the two values, rounding the result down
  (-\) :: a -> a -> a
  -- | Multiply the two values, rounding the result up
  (*/) :: a -> a -> a
  -- | Multiply the two values, rounding the result down
  (*\) :: a -> a -> a

  prev x     = x - eps x
  next x     = x + eps x
  x +/ y     = next $ x + y
  x +\ y     = prev $ x + y
  x -/ y     = next $ x - y
  x -\ y     = prev $ x - y
  x */ y     = next $ x * y
  x *\ y     = prev $ x * y

instance ExplicitRounding Int where
  eps = const 0

instance (Integral a, ExplicitRounding a) => ExplicitRounding (Ratio a) where
  eps x = (eps $ numerator x) % (abs (denominator x) - (eps $ denominator x))

instance ExplicitRounding Float where
  eps 0 = eps $ 2e-36
  eps x = encodeFloat 2 (snd $ decodeFloat x)

instance ExplicitRounding Double where
  eps 0 = eps $ 1e-300
  eps x = encodeFloat 2 (snd $ decodeFloat x)
