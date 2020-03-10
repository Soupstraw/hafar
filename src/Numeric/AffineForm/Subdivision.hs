{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.AffineForm.Subdivision where

import Control.Monad.Reader
import Control.Lens

import Data.List

import Numeric.Interval as IA

class Subdivisible a where
  subdivide :: a -> Int -> [a]
  combine2  :: a -> a -> a
  combine   :: [a] -> a

  combine2 l r = combine [l,r]
  combine      = foldl1 combine2

instance (Ord a, Fractional a) => Subdivisible (Interval a) where
  subdivide i n = f . fromIntegral <$> [1..n]
    where f x = ((x-1)*w/n')IA....(x*w/n')
          w   = width i
          n'  = fromIntegral n
  combine2 = hull

instance (Subdivisible a) => Subdivisible [a] where
  subdivide l n = sequenceA $ flip subdivide n <$> l
  combine2 l r  = uncurry combine2 <$> zip l r

data SubdivisionEnvironment a b = SubdivisionEnvironment
  { _function :: a -> a
  , _errorFun :: a -> b
  , _maxError :: b
  , _maxDepth :: Int
  }
makeLenses ''SubdivisionEnvironment

type Subdivider a e = Reader (SubdivisionEnvironment a e) (SubdivisionTree a)

data SubdivisionTree a
  = Branch (SubdivisionTree a)
  | Node a

singletonTree :: a -> SubdivisionTree a
singletonTree afm = Node afm

branchAndBound :: SubdivisionTree a -> Subdivider a e
branchAndBound (Node a) = undefined

