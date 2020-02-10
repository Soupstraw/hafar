{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Numeric.AffineForm.Utils (
                                embed, sumup,
                                pmod', clamp,
                                eps,
                                ExplicitRounding,
                                (+/), (+\),
                                (-/), (-\),
                                (*/), (*\)
                                ) where

import qualified Numeric.Interval as IA
import Numeric.Rounded
import Data.Ratio
import Data.Fixed
import Data.Proxy
import GHC.Float hiding (clamp)

embed :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
embed x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0


pmod' :: (Ord a, RealFrac a) => a -> a -> a
pmod' a b
  | a < 0 = pmod' (a + b*(fromIntegral . ceiling . abs $ a/b)) b
  | otherwise = a `mod'` b

clamp :: (Ord a) => a -> a -> a -> a
clamp x a b = min (max a x) b

sumup :: (ExplicitRounding a) => [a] -> a
sumup = foldl (+/) 0

-- Rounding

class (Ord a, Num a) => ExplicitRounding a where
  eps :: a -> a
  interval :: a -> IA.Interval a
  prev :: a -> a
  next :: a -> a
  (+/) :: a -> a -> a
  (+\) :: a -> a -> a
  (-/) :: a -> a -> a
  (-\) :: a -> a -> a
  (*/) :: a -> a -> a
  (*\) :: a -> a -> a

  -- Epsilon should be defined so that `interval x` would contain all the values that
  -- could be equal to x due to rounding errors
  prev x     = x - eps x
  next x     = x + eps x
  interval x = (prev x)IA....(next x)
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
  eps 0 = eps $ 1e-36
  eps x = encodeFloat 1 (snd $ decodeFloat x)

instance ExplicitRounding Double where
  eps 0 = eps $ 1e-300
  eps x = encodeFloat 1 (snd $ decodeFloat x)
