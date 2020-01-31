module Numeric.AffineForm (AFM, AF, newEps,
                           newFromInterval,
                           radius,
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

-- | An affine form is defined by its midpoint, list of epsilon coefficients and an error coefficient
data AF a
  = AF a [a] a
  deriving (Show)

data Curvature = Convex | Concave

data AFException
  = DivisionByZero
  | LogFromNegative

instance Show AFException where
  show DivisionByZero = "division by zero"
  show LogFromNegative = "logarithm from a negative number"

instance Exception AFException

instance (Num a, Ord a, Arbitrary a) => Arbitrary (AF a) where
  arbitrary = do
                x <- arbitrary
                xs <- arbitrary
                (Positive xe) <- arbitrary
                return $ AF x xs xe
  shrink (AF x xs xe) =
    [AF x' xs' xe' | (x', xs', xe') <- shrink (x, xs, xe)]

instance (Fractional a, Ord a) => Num (AF a) where
  (+) = add
  (*) = multiply
  abs = absAF
  signum = signumAF
  fromInteger x = AF (fromInteger x) [] 0
  -- TODO: singleton (fromInteger x)
  negate = negateAF

instance (Fractional a, Ord a) => Fractional (AF a) where
  recip = recipAF
  fromRational x = AF (fromRational x) [] 0
  -- TODO: singleton (fromRational x)

instance (Floating a, RealFrac a, Ord a) => Floating (AF a) where
  pi = AF pi [] 0 -- TODO "fromIntervalAnonymous" using upper and lower bound on pi
  -- Prelude> decodeFloat pi
-- (7074237752028440,-51)
-- Error term is: encodeFloat 1 $ -51
-- Type class needed: RealFloat

  exp = minrange exp exp Convex
  log x
    | lo x > 0  = minrange log recip Concave x
    | otherwise = throw LogFromNegative
  sin = sinAF
  cos = cosAF
  asin = minrange asin (\x -> 1/sqrt (1-x^2)) undefined
  acos = minrange acos (\x -> -1/sqrt (1-x^2)) undefined
  atan = minrange atan (\x -> 1/(x^2+1)) undefined
  sinh = minrange sinh cosh undefined
  cosh = minrange cosh sinh Convex
  asinh = minrange asinh (\x -> 1/sqrt (x^2+1)) undefined
  acosh = minrange acosh (\x -> 1/((sqrt (x-1))*(sqrt (x+1)))) Concave
  atanh = minrange atanh (\x -> 1/(1-x^2)) undefined

type AFIndex = Int

-- | AFM is a state monad that ensures that any new noise symbols have not been used by any previous affine form.
-- All affine arithmetic calculations should be done inside the AFM monad. Affine forms do not make sense outside of their monad context.
type AFM = State AFIndex

-- TODO: Currently, AFM has one type parameter, the return value. But it should probably also be parametrized
--       by the number type. So "type AFM a = State AFIndex"

-- | This gives an affine form with midpoint 0 and radius 1.
-- This affine form does not share epsilons with any affine forms created before it.
-- It can be used to instantiate new affine forms.
newEps :: Num a => AFM (AF a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AF 0 (replicate idx 0 ++ [1]) 0

-- | Creates a new affine form that covers the interval.
-- This affine form does not share epsilons with any affine forms created before it.
newFromInterval :: (Eq a, Fractional a) => IA.Interval a -> AFM (AF a)
newFromInterval i = do
  eps <- newEps
  let mult = ((IA.width i) / 2) .* eps
  return $ (IA.midpoint i) .+ mult

-- TODO
-- Define function: nextNumber, prevNumber (in type class RealFloat) such that prevNumber x < x < nextNumber x.
-- Define function: errorBound such that errorBound x >= |x - prevNumber x|, |x - nextNumber x|.

-- TODO:
-- define type class "ExplicitRounding"
-- should have nextNumber etc.
-- instance RealFloat => ExplicitRounding

-- TODO
-- Not sound because / gives wrong rounding
-- Better:
--   (x,y) = high/low point
--   midpoint = (x+y) / 2
--   error = max (midpoint - x) (y - midpoint)
-- Maybe still wrong? Is "-" precise? Probably not in general.


-- TODO: define helper functions with clear rounding behavior:
-- E.g.: plusLow x y = (best) lower bound on x+y
-- (Some of them with additional assumptions if needed, e.g., "y >= 0")
-- absHigh, etc.


-- | Gives the radius of the affine form
radius :: (Num a) => AF a -> a
radius (AF _ xs xe) = xe + (sum $ abs <$> xs)

-- | Gives the midpoint of the affine form (the first term of the affine form).
midpoint :: AF a -> a
midpoint (AF x _ _) = x

-- | Gives the minimal possible value of the affine form
lo :: (Num a) => AF a -> a
lo af = (midpoint af) - (radius af)

-- | Gives the maximal possible value of the affine form
hi :: (Num a) => AF a -> a
hi af = (midpoint af) + (radius af)

-- | Gives the corresponding interval of the affine form
interval :: (Num a, Ord a) => AF a -> IA.Interval a
interval af = (lo af)...(hi af)

-- | Returns whether the element is representable by the affine form
member :: (Num a, Ord a) => a -> AF a -> Bool
member x af = x `IA.member` (interval af)

-- Affine arithmetic operations

-- | Sets the midpoint of the affine form
setMidpoint :: a -> AF a -> AF a
setMidpoint m (AF x xs xe) = AF m xs xe

-- | Adds the value to the error term of the affine form
addError :: (Num a) => AF a -> a -> AF a
addError (AF x xs xe) e = AF x xs (xe + e)

-- | Adds a scalar value to the affine form
(.+) :: (Num a) => a -> AF a -> AF a
a .+ (AF x xs xe) = AF (x+a) xs xe

add :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `add` (AF y ys ye) = AF (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> embed xs ys

negateAF :: (Num a) => AF a -> AF a
negateAF (AF x xs xe) = AF (-x) (negate <$> xs) xe

multiply :: (Num a) => AF a -> AF a -> AF a
(AF x xs xe) `multiply` (AF y ys ye) = AF (x*y) zs (ze+ze2)
  where zs = uncurry (+) <$> embed ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) (abs <$> xs ++ [xe]) (abs <$> ys ++ [ye])
        ze2 = (abs x*ye) + (abs y*xe)

-- | Multiplies the affine form by a scalar
(.*) :: (Eq a, Num a) => a -> AF a -> AF a
a .* (AF x xs xe) = AF (a*x) ((a*) <$> xs) $ a * xe

recipAF :: (Ord a, Fractional a) => AF a -> AF a
recipAF af =
  -- Any way to get rid of the if-else statements?
  if low > 0
    then minrange recip (\x -> -1/x^2) Convex af
    else if high < 0
      then negateAF . recipAF $ negateAF af
      else throw DivisionByZero
  where high = hi af
        low  = lo af

cosAF :: (Ord a, RealFrac a, Floating a) => AF a -> AF a
cosAF af
  | radius af < pi = f af
  | otherwise = AF 0 [] 1
  where a = lo af `pmod'` (2*pi)
        b = hi af `pmod'` (2*pi)
        f x
          -- function never reaches extremum
          | a < pi && b < pi || a > pi && b > pi = minrange cos (negate . sin) undefined af
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

-- | Fixes the epsilons of the affine form to the values in the list
-- The list will be extended by zeroes to match the length of the list of coefficients
fix :: (Num a, Ord a) => AF a -> [a] -> IA.Interval a
fix (AF x xs xe) vals = (m - xe)...(m + xe)
  where em = embed xs vals
        prod = uncurry (*) <$> em
        m  = x + sum prod

-- | Returns a min-range approximation function for given function and its derivative.
minrange :: (Fractional a, Ord a) => (a -> a) -> (a -> a) -> Curvature -> (AF a -> AF a)
-- TODO: inputs should be (a -> a*a), returning lower/upper bound
-- TODO: think where you have to use upper/lower rounded values
minrange f f' curv = \af ->
  let a = hi af
      b = lo af
      p = case curv of
            Convex  -> f' a
            Concave -> f' b
      q = ((f a)+(f b)-p*(a+b))/2
      d = abs ((f a)-(f b)+p*(a-b))/2
  in
  q .+ (p .* af) `addError` d

-- | Evaluates the monad after applying a function
evalWith :: (a -> b) -> AFM a -> b
evalWith f afm = evalState (afm >>= return . f) 0



-- TODO: other possibility to define plusHighLow:
-- plusHighLow x y = evalRoundingMonad do setRoundingUp
--                                        let r1 = x + y
--                                        setRoundingDown
--                                        let r2 = x + y
--                                        return (r1,r2)

myNumber = evalWith id (do x <- singleton 1; y <- exp 1; z <- midpoint y; return z)

data AF t a
  = AF a [a] a
  deriving (Show)

type AFM t a = State AFIndex

eval = (\f -> evalState f 0) :: ((forall t. AFM t a r) -> r)
