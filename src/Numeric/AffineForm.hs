module Numeric.AffineForm (AFM, AF, newEps,
                           newFromInterval,
                           range,
                           midpoint,
                           lo, hi,
                           interval,
                           member,
                           setMidpoint,
                           fix,
                           (.+), (.*)
                          ) where

import Control.Monad.State hiding (fix)
import Control.Exception (Exception, throw, evaluate, try)

import Numeric.AffineForm.Utils
import qualified Numeric.Interval as IA
import Numeric.Interval ((...))
import Data.Fixed (mod')
import Data.Ratio (approxRational, (%))
import Data.Either (fromLeft, fromRight)

import Test.QuickCheck

data AF a
  = AF a [a] a
  deriving (Show)

data AFException
  = DivisionByZero
  | LogFromNegative

instance Show AFException where
  show DivisionByZero = "division by zero"
  show LogFromNegative = "logarithm from a negative number"

instance Exception AFException

instance (Num a, Arbitrary a) => Arbitrary (AF a) where
  arbitrary = do
                x <- arbitrary
                xs <- arbitrary
                xe <- arbitrary
                return $ AF x xs xe
  shrink (AF x xs xe) =
    [AF x' xs' xe' | (x', xs', xe') <- shrink (x, xs, xe)]

instance (Fractional a, Ord a) => Num (AF a) where
  (+) = add
  (*) = multiply
  abs = absAF
  signum = signumAF
  fromInteger x = AF (fromInteger x) [] 0
  negate = negateAF

instance (Fractional a, Ord a) => Fractional (AF a) where
  recip = recipAF
  fromRational x = AF (fromRational x) [] 0

instance (Floating a, RealFrac a, Ord a) => Floating (AF a) where
  pi = AF pi [] 0
  exp = expAF
  log = logAF
  sin = sinAF
  cos = cosAF
  asin = undefined
  acos = undefined
  atan = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

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
  let mult = ((IA.width i) / 2) .* eps
  return $ (IA.midpoint i) .+ mult

range :: (Num a) => (AF a) -> a
range (AF _ xs xe) = xe + (sum $ abs <$> xs)

midpoint :: AF a -> a
midpoint (AF x _ _) = x

lo :: (Num a) => AF a -> a
lo af = (midpoint af) - (range af)

hi :: (Num a) => AF a -> a
hi af = (midpoint af) + (range af)

interval :: (Num a, Ord a) => AF a -> IA.Interval a
interval af = (lo af)...(hi af)

member :: (Num a, Ord a) => a -> AF a -> Bool
member x af = x `IA.member` (interval af)

-- Affine arithmetic operations

setMidpoint :: a -> AF a -> AF a
setMidpoint m (AF x xs xe) = AF m xs xe

addError :: (Num a) => AF a -> a -> AF a
addError (AF x xs xe) e = AF x xs (xe + e)

-- Add scalar
(.+) :: (Num a) => a -> AF a -> AF a
a .+ (AF x xs xe) = AF (x+a) xs xe

add :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `add` (AF y ys ye) = AF (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> embed xs ys

negateAF :: (Num a) => AF a -> AF a
negateAF (AF x xs xe) = AF (-x) (negate <$> xs) xe

multiply :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `multiply` (AF y ys ye) = AF (x*y) zs ze
  where zs = (\(l,r) -> l+r) <$> embed ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) (abs <$> xs ++ [xe]) (abs <$> ys ++ [ye])

(.*) :: (Eq a, Num a) => a -> AF a -> AF a
a .* (AF x xs xe) = AF (a*x) ((a*) <$> xs) xe

recipAF :: (Ord a, Fractional a) => AF a -> AF a
recipAF af =
  -- Any way to get rid of the if-else statements?
  if low > 0
    then minrange recip (\x -> -1/x^2) af
    else if high < 0
      then negateAF . recipAF $ negateAF af
      else throw DivisionByZero
  where high = hi af
        low  = lo af

divide :: (Ord a, Fractional a) => AF a -> AF a -> AF a
x `divide` y = x `multiply` (recipAF y)

logAF :: (Ord a, Floating a) => AF a -> AF a
logAF af =
  if lo af > 0
    then minrange log recip af
    else throw LogFromNegative

expAF :: (Ord a, Floating a) => AF a -> AF a
expAF af = minrange exp exp af

cosAF :: (Ord a, RealFrac a, Floating a) => AF a -> AF a
cosAF af
  | range af < pi = f af
  | otherwise = AF 0 [] 1
  where a = lo af `pmod'` (2*pi)
        b = hi af `pmod'` (2*pi)
        f x
          -- function never reaches extremum
          | a < pi && b < pi || a > pi && b > pi = minrange cos (negate . sin) af
          -- function reaches extremum exactly once
          | a < b = AF (rl - 1) [] rl
          -- function reaches extremum more than once
          | otherwise = AF (1 - rh) [] rh
          where rl = abs (1 + (max (cos a) (cos b)))/2
                rh = abs (1 - (min (cos a) (cos b)))/2

sinAF :: (Ord a, RealFrac a, Floating a) => AF a -> AF a
sinAF af = cosAF ((-pi/2) .+ af)

absAF :: (Ord a, Fractional a) => AF a -> AF a
absAF af
  | lo af >= 0 = af
  | hi af <= 0 = -af
  | otherwise = AF x [] x
    where x = (max (abs . hi $ af) (abs . lo $ af))/2

signumAF :: (Ord a, Num a) => AF a -> AF a
signumAF af
  | lo af >= 0 = AF 1 [] 0
  | hi af <= 0 = AF (-1) [] 0
  | otherwise = AF 0 [] 1

--
-- Helper functions
--

fix :: (Num a, Ord a) => AF a -> [a] -> IA.Interval a
fix (AF x xs xe) vals = (m - xe)...(m + xe)
  where em = embed xs vals
        prod = uncurry (*) <$> em
        m  = x + sum prod

-- Returns a min-range approximation function for given function and its derivative
minrange :: (Fractional a, Ord a) => (a -> a) -> (a -> a) -> (AF a -> AF a)
minrange f f' = \af ->
  let a = hi af
      b = lo af
      p = f' a
      q = ((f a)+(f b)-p*(a+b))/2
      d = abs ((f a)-(f b)+p*(a-b))/2
  in
  q .+ (p .* af) `addError` d
