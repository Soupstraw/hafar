{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Numeric.AffineForm.Utils (
                                embed,
                                pmod', clamp,
                                epsilon,
                                ExplicitRounding
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

-- Rounding

class (Ord a, Num a) => ExplicitRounding a where
  epsilon :: a -> a
  interval :: a -> IA.Interval a

  -- Epsilon should be defined so that `interval x` would contain all the values that
  -- could be set equal to x due to rounding errors
  interval x = (x - epsilon x)IA....(x + epsilon x)

instance ExplicitRounding Int where
  epsilon _ = 0

instance ExplicitRounding Float where
  epsilon 0 = epsilon $ 1e-36
  epsilon x = encodeFloat 1 (snd $ decodeFloat x)

instance ExplicitRounding Double where
  epsilon 0 = epsilon $ 1e-300
  epsilon x = encodeFloat 1 (snd $ decodeFloat x)
