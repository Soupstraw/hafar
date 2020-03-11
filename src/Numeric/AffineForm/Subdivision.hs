{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Subdivision contains an implementation of the branch-and-bound algorithm.
-- This method divides interval-like values into smaller subdivisions and then applies a function to those values.
-- This results in a more accurate output at the cost of having to repeat the calculations multiple times
module Numeric.AffineForm.Subdivision
  ( Subdivisible(..)
  , SubdivisionEnvironment(..)
  , defaultEnvironment
  , branchAndBound
  ) where

import Control.Monad.Reader
import Control.Monad.State

import Data.List

import Numeric.Interval as IA

-- | The 'Subdivisible' class is used for datatypes that can be broken down into smaller pieces
-- and combined together
--
-- The 'subdivide' function subdivides the value into `n` smaller values and 'combine' joins
-- the subdivisions together into a bigger value
--
-- Subdividing and then combining a value is expected to give the initial value (sans rounding errors)
class Subdivisible a where
  subdivide    :: a -> Int -> [a]
  combine2     :: a -> a -> a
  combine      :: [a] -> a

  combine2 l r = combine [l,r]
  combine      = foldl1 combine2

instance (Ord a, Fractional a) => Subdivisible (Interval a) where
  subdivide i n = f . fromIntegral <$> [1..n]
    where f x = (((x-1)*w/n')IA....(x*w/n')) + (IA.singleton $ inf i)
          w   = width i
          n'  = fromIntegral n
  combine2 = hull

instance (Subdivisible a) => Subdivisible [a] where
  subdivide l n = sequenceA $ flip subdivide n <$> l
  combine2 l r  = uncurry combine2 <$> zip l r

-- | A data structure for configuring the 'branchAndBound' method
--
-- 'function' is the function that gets evaluated
-- 'errorFun' is the error measuring function of the result
-- 'maxError' specifies the maximum permitted error of an evaluation
-- 'maxDepth' specifies how deep the subdivision can go
-- 'subdivs'  specifies the number of subdivisions per value
data SubdivisionEnvironment a b e = SubdivisionEnvironment
  { function :: a -> b
  , errorFun :: b -> e
  , maxError :: e
  , maxDepth :: Int
  , subdivs  :: Int
  }

-- | This function creates a simple subdivision configuration.
-- It requires the evaluator function and an error measuring function as its parameters.
defaultEnvironment :: (Fractional e) => (a -> b) -> (b -> e) -> SubdivisionEnvironment a b e
defaultEnvironment f g = SubdivisionEnvironment
  { function = f
  , errorFun = g
  , maxError = 0.1
  , maxDepth = 3
  , subdivs  = 2
  }

type Subdivider a b e = Reader (SubdivisionEnvironment a b e) (SubdivisionTree a)

data SubdivisionTree a
  = Branch [SubdivisionTree a]
  | Node a
  deriving (Show, Functor)

deepen :: (Subdivisible a, Ord e) => SubdivisionTree a -> Int -> Subdivider a b e
deepen (Node x) depth =
  do
    env <- ask
    let res = function env $ x
        err = errorFun env $ res
        n   = subdivs env
    if err <= maxError env
       -- Accurate answer found
       then return $ Node x
       -- Subdivide and deepen
       else deepen (Branch $ Node <$> subdivide x n) $ depth
deepen (Branch l) depth =
  do
    env <- ask
    if depth >= maxDepth env
       then return $ Branch l
       else Branch <$> (sequence $ flip deepen (depth + 1) <$> l)

collapse :: (Subdivisible a) => SubdivisionTree a -> a
collapse (Node a)   = a
collapse (Branch l) = combine $ collapse <$> l

evalTree :: (Subdivisible b) => Subdivider a b e -> SubdivisionEnvironment a b e -> b
evalTree div cfg = flip runReader cfg $
  do
    val <- div
    env <- ask
    return . collapse $ function env <$> val

-- | This function iteratively subdivides a value to arrive at a more accurate result
branchAndBound :: (Subdivisible a, Subdivisible b, Ord e) => a -> SubdivisionEnvironment a b e -> b
branchAndBound x env = flip evalTree env $ deepen (Node x) 0

test :: Interval Double
test =
  flip evalTree env $ deepen (Node $ [3 IA.... 4, 1 IA....4]) 0
  where env = SubdivisionEnvironment
          { function = (\[x, y] -> x * y - x)
          , errorFun = width
          , maxError = 0.1
          , maxDepth = 5
          , subdivs  = 2
          }

