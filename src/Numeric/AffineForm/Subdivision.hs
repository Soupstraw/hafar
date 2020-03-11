{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Numeric.AffineForm.Subdivision
  ( Subdivisible(..)
  , defaultEnvironment
  , branchAndBound
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Data.List

import Numeric.Interval as IA

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

data SubdivisionEnvironment a b e = SubdivisionEnvironment
  { _function :: a -> b
  , _errorFun :: b -> e
  , _maxError :: e
  , _maxDepth :: Int
  , _subdivs  :: Int
  }
makeLenses ''SubdivisionEnvironment
defaultEnvironment :: (Num e) => SubdivisionEnvironment a b e
defaultEnvironment = SubdivisionEnvironment
  { _function = undefined
  , _errorFun = undefined
  , _maxError = 0
  , _maxDepth = 3
  , _subdivs  = 2
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
    let res = env ^. function $ x
        err = env ^. errorFun $ res
        n   = env ^. subdivs
    if err <= env ^. maxError
       -- Accurate answer found
       then return $ Node x
       -- Subdivide and deepen
       else deepen (Branch $ Node <$> subdivide x n) $ depth
deepen (Branch l) depth =
  do
    env <- ask
    if depth >= env^.maxDepth
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
    return . collapse $ env ^. function <$> val

branchAndBound :: (Subdivisible a, Subdivisible b, Ord e) => a -> SubdivisionEnvironment a b e -> b
branchAndBound x env = flip evalTree env $ deepen (Node x) 0

test :: Interval Double
test =
  flip evalTree env $ deepen (Node $ [3 IA.... 4, 1 IA....4]) 0
  where env = SubdivisionEnvironment
          { _function = (\[x, y] -> x * y - x)
          , _errorFun = width
          , _maxError = 0.1
          , _maxDepth = 5
          , _subdivs  = 2
          }

