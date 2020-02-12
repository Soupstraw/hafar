{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- | Provides some useful functions
module Numeric.AffineForm.Utils (
                                embed, sumup,
                                pmod', clamp,
                                ) where

import Data.Fixed
import Numeric.AffineForm.ExplicitRounding

-- | Zips the two lists together, padding the shorter list with zeroes
embed :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
embed x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0


-- | Sawtooth function with period `b`
pmod' :: (Ord a, RealFrac a) => a -> a -> a
pmod' a b
  | a < 0 = pmod' (a + b*(fromIntegral . ceiling . abs $ a/b)) b
  | otherwise = a `mod'` b

-- | Clamps `x` between values `a` and `b`
clamp :: (Ord a) => a -> a -> a -> a
clamp x a b = min (max a x) b

-- | Like sum but rounds all values up
sumup :: (ExplicitRounding a) => [a] -> a
sumup = foldl (+/) 0
