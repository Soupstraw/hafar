module Numeric.AffineForm.Utils where

import qualified Numeric.Interval as IA

import Data.Fixed

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
