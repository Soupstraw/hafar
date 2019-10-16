module AffineForm where

import Control.Monad.State

data AffineForm a =
  AffineForm a [a]

zipWithZeros :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
zipWithZeros x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0
