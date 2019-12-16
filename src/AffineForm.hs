module AffineForm (newEps,
                   newFromInterval,
                   range,
                   midpoint,
                   lo, hi,
                   interval,
                   negateAF,
                   recipAF,
                   log, min, max,
                   abs, sin, cos,
                   tan
                  ) where

import Control.Monad.State

import qualified Numeric.Interval as IA
import Numeric.Interval ((...))
import Data.Maybe

import System.Random
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

data AF a
  = AF a [a] a
  | Reals
  deriving (Show)

instance (Num a, Arbitrary a) => Arbitrary (AF a) where
  arbitrary = do
    oneof [ do
              x <- arbitrary
              xs <- arbitrary
              return $ AF x xs 0
          , return Reals
          ]
  shrink Reals = []
  shrink (AF x xs xe) =
    [Reals] ++
    [AF x' xs' xe' | (x', xs', xe') <- shrink (x, xs, xe)]

instance Bounded Float where
  minBound = -(1/0)
  maxBound = 1/0

instance Bounded Double where
  minBound = -(1/0)
  maxBound = 1/0

type AFIndex = Int
type AFM = State AFIndex

newEps :: Num a => AFM (AF a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AF 0 (replicate idx 0 ++ [1]) 0

newFromInterval :: (Eq a, Fractional a) => IA.Interval a -> AFM (AF a)
newFromInterval i = do
  eps <- newEps
  let mult = ((IA.width i) / 2) `multiplyScalar` eps
  return $ (IA.midpoint i) `addScalar` mult

range :: (Num a, Bounded a) => AF a -> a
range (AF _ xs xe) = xe + (sum $ abs <$> xs)
range Reals = maxBound

midpoint :: (Num a) => AF a -> Maybe a
midpoint (AF x _ _) = return x
midpoint Reals = Nothing

lo :: (Num a, Bounded a) => AF a -> a
lo Reals = minBound
lo af = (fromMaybe 0 $ midpoint af) - (range af)

hi :: (Num a, Bounded a) => AF a -> a
hi Reals = maxBound
hi af = (fromMaybe 0 $ midpoint af) + (range af)

interval :: (Num a, Ord a, Bounded a) => AF a -> IA.Interval a
interval af = (lo af)...(hi af)

-- Affine arithmetic operations

zeroAF :: (Num a) => AF a
zeroAF = AF 0 [] 0

addError :: (Num a) => AF a -> a -> AF a
addError (AF x xs xe) e = AF x xs (xe + e)

-- Add scalar
addScalar :: (Num a) => a -> AF a -> AF a
a `addScalar` (AF x xs xe) = AF (x+a) xs xe
_ `addScalar` Reals = Reals

add :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `add` (AF y ys ye) = AF (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> zipWithZeros xs ys
Reals `add` _ = Reals
_ `add` Reals = Reals

negateAF :: (Num a) => AF a -> AF a
negateAF (AF x xs xe) = AF (-x) (negate <$> xs) xe
negateAF Reals = Reals

subtract :: (Num a) => AF a -> AF a -> AF a
x `subtract` y = x `add` (negateAF y)

multiply :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `multiply` (AF y ys ye) = AF (x*y) zs ze
  where zs = (\(l,r) -> l+r) <$> zipWithZeros ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) (xs ++ [xe]) (ys ++ [ye])
Reals `multiply` _ = Reals
_ `multiply` Reals = Reals

multiplyScalar :: (Eq a, Num a) => a -> AF a -> AF a
a `multiplyScalar` (AF x xs xe) = AF (a*x) ((a*) <$> xs) xe
0 `multiplyScalar` Reals = zeroAF
_ `multiplyScalar` Reals = Reals

recipAF :: (Ord a, Fractional a, Bounded a) => AF a -> Maybe (AF a)
recipAF af = do
  let high = hi af
      low  = lo af
  -- Any way to get rid of the if-else statements?
  if low > 0
    then return $ recipAF' af
    else if high < 0
      then negateAF <$> (recipAF $ negateAF af)
      else Nothing

recipAF' :: (Ord a, Fractional a, Bounded a) => AF a -> AF a
recipAF' af = do
  let a = hi af
      b = lo af
      p = -1/b^2
      q = -p*(a+b)^2/(2*a)
      d = -p*(a-b)^2/(2*a)
  q `addScalar` (p `multiplyScalar` af) `addError` d

divide :: (Ord a, Fractional a, Bounded a) => AF a -> AF a -> Maybe (AF a)
x `divide` y = (x `multiply`) <$> (recipAF y)

(**) :: (Num a) => AF a -> a -> AF a
(AF x xs xe) ** y = undefined

-- Helper functions

zipWithZeros :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
zipWithZeros x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0

-- QuickCheck properties
data Approx a = Approx a (AF a)
  deriving (Show)

valid :: (Ord a, Num a, Bounded a) => Approx a -> Bool
valid (Approx x af) = x `IA.member` i
  where i = interval af

instance (Ord a, Num a, Arbitrary a, Bounded a, Random a) => Arbitrary (Approx a) where
  arbitrary = do
    af <- arbitrary
    let i = interval af
    x <- choose (IA.inf i, IA.sup i)
    return $ Approx x af
  shrink (Approx x af) = filter valid [Approx x' af' | (x', af') <- shrink (x, af)]

prop_fiaa_addition :: Approx Int -> Approx Int -> Bool
prop_fiaa_addition (Approx a x) (Approx b y) = (a + b) `IA.member` i
  where i = interval $ x `add` y
