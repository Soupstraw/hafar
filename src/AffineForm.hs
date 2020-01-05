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

import Control.Monad.State hiding (fix)
import Control.Exception as Exception

import qualified Numeric.Interval as IA
import Numeric.Interval ((...))
import Data.Fixed (mod')
import Data.Ratio (approxRational, (%))

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

data AF a
  = AF a [a] a
  deriving (Show)

data AFException
  = DivisionByZero

instance Show AFException where
  show DivisionByZero = "division by zero"

instance Exception AFException

instance (Num a, Arbitrary a) => Arbitrary (AF a) where
  arbitrary = do
                x <- arbitrary
                xs <- arbitrary
                return $ AF x xs 0
  shrink (AF x xs xe) =
    [AF x' xs' xe' | (x', xs', xe') <- shrink (x, xs, xe)]

instance (Num a) => Num (AF a) where
  (+) = add
  (*) = multiply
  abs = undefined    -- TODO
  signum = undefined -- TODO
  fromInteger x = AF (fromInteger x) [] 0
  negate = negateAF

instance (Fractional a, Ord a) => Fractional (AF a) where
  recip = recipAF
  fromRational x = AF (fromRational x) [] 0

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

range :: (Num a) => AF a -> a
range (AF _ xs xe) = xe + (sum $ abs <$> xs)

midpoint :: AF a -> a
midpoint (AF x _ _) = x

lo :: (Num a) => AF a -> a
lo af = (midpoint af) + (range af)

hi :: (Num a) => AF a -> a
hi af = (midpoint af) + (range af)

interval :: (Num a, Ord a) => AF a -> IA.Interval a
interval af = (lo af)...(hi af)

member :: (Num a, Ord a) => a -> AF a -> Bool
member x af = x `IA.member` (interval af)

-- Affine arithmetic operations

addError :: (Num a) => AF a -> a -> AF a
addError (AF x xs xe) e = AF x xs (xe + e)

-- Add scalar
addScalar :: (Num a) => a -> AF a -> AF a
a `addScalar` (AF x xs xe) = AF (x+a) xs xe

add :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `add` (AF y ys ye) = AF (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> embed xs ys

negateAF :: (Num a) => AF a -> AF a
negateAF (AF x xs xe) = AF (-x) (negate <$> xs) xe

multiply :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `multiply` (AF y ys ye) = AF (x*y) zs ze
  where zs = (\(l,r) -> l+r) <$> embed ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) (abs <$> xs ++ [xe]) (abs <$> ys ++ [ye])

multiplyScalar :: (Eq a, Num a) => a -> AF a -> AF a
a `multiplyScalar` (AF x xs xe) = AF (a*x) ((a*) <$> xs) xe

recipAF :: (Ord a, Fractional a) => AF a -> AF a
recipAF af = do
  let high = hi af
      low  = lo af
  -- Any way to get rid of the if-else statements?
  if low > 0
    then recipAF' af
    else if high < 0
      then negateAF . recipAF $ negateAF af
      else Exception.throw DivisionByZero

recipAF' :: (Ord a, Fractional a) => AF a -> AF a
recipAF' af = do
  let a = hi af
      b = lo af
      p = -1/b^2
      q = -p*(a+b)^2/(2*a)
      d = -p*(a-b)^2/(2*a)
  q `addScalar` (p `multiplyScalar` af) `addError` d

divide :: (Ord a, Fractional a) => AF a -> AF a -> AF a
x `divide` y = x `multiply` (recipAF y)

-- Helper functions

embed :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
embed x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0

fix :: (Num a, Ord a) => AF a -> [a] -> IA.Interval a
fix (AF x xs xe) vals = (m - xe)...(m + xe)
  where em = embed xs vals
        prod = uncurry (*) <$> em
        m  = x + sum prod

-- QuickCheck properties
data Approx a = Approx a (AF a)
  deriving (Show)

data EpsV = EpsV [Rational]
  deriving (Show)

instance Arbitrary EpsV where
  arbitrary = do
    l <- listOf arbitrary
    return . EpsV $ (\x -> (x `mod'` 1) * 2 - 1) <$> l
  shrink (EpsV l) = filter validEV $ EpsV <$> (shrink l)

valid :: (Ord a, Num a) => Approx a -> Bool
valid (Approx x af) = x `IA.member` i
  where i = interval af

validEV :: EpsV -> Bool
validEV (EpsV l) = all (\x -> -1 <= x && x <= 1) l

instance (Ord a, Num a, Arbitrary a, Random a) => Arbitrary (Approx a) where
  arbitrary = do
    af <- arbitrary
    let i = interval af
    x <- choose (IA.inf i, IA.sup i)
    return $ Approx x af
  shrink (Approx x af) = filter valid [Approx x' af' | (x', af') <- shrink (x, af)]

-- RuKaS14.pdf [1102:2]
prop_addition :: EpsV -> AF Rational -> AF Rational -> Bool
prop_addition (EpsV e) x y = x `fix` e + y `fix` e == (x + y) `fix` e

prop_subtraction :: EpsV -> AF Rational -> AF Rational -> Bool
prop_subtraction (EpsV e) x y = x `fix` e - y `fix` e == (x - y) `fix` e

prop_exponentiation :: EpsV -> AF Rational -> Small Int -> Property
prop_exponentiation (EpsV e) x y = counterexample str res
  where n = (abs $ getSmall y) `mod` 3
        res = (x ^ n) `fix` e `IA.contains` ((x `fix` e) ^ n)
        str = (show $ (x ^ n) `fix` e) ++ "\n" ++ (show $ ((x `fix` e) ^ n))
