{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module Numeric.AffineForm (AFM, AF, newEps,
                           newFromInterval,
                           singleton,
                           evalAFM,
                           radius,
                           midpoint,
                           inf, sup,
                           interval,
                           member,
                           epscount_,
                           setMidpoint,
                           fix,
                           addError,
                           (.+), (.*)
                          ) where

import Control.Monad.State hiding (fix)
import Control.Monad.Identity hiding (fix)
import Control.Exception (Exception, throw, evaluate, try)

import Numeric.AffineForm.Utils
import Numeric.AffineForm.ExplicitRounding
import qualified Numeric.Interval as IA
import Numeric.Interval ((...))
import Data.Fixed (mod')
import Data.Ratio (approxRational, (%))
import Data.Either (fromLeft, fromRight)

import Test.QuickCheck

-- | An affine form is defined by its midpoint, list of epsilon coefficients and an error coefficient
data AF s a
  = AF a [a] a
  deriving (Show)

data Curvature = Convex | Concave

data AFException
  = DivisionByZero
  | LogFromNegative
  | AddingNegativeError

instance Show AFException where
  show DivisionByZero = "division by zero"
  show LogFromNegative = "logarithm from a negative number"
  show AddingNegativeError = "cannot add a negative error to an affine form"

instance Exception AFException

instance (Num a, Ord a, Arbitrary a) => Arbitrary (AF s a) where
  arbitrary = do
                x <- arbitrary
                xs <- arbitrary
                (Positive xe) <- arbitrary
                return $ AF x xs xe
  shrink (AF x xs xe) =
    [AF x' xs' xe' | (x', xs', xe') <- shrink (x, xs, xe)]

instance (Fractional a, ExplicitRounding a, Ord a) => Num (AF s a) where
  (+) = add
  (*) = multiply
  abs = absAF
  signum = signumAF
  fromInteger = singleton . fromInteger
  negate = negateAF

instance (Fractional a, ExplicitRounding a, Ord a) => Fractional (AF s a) where
  recip = recipAF
  fromRational = singleton . fromRational

instance (Floating a, RealFrac a, ExplicitRounding a, Ord a) => Floating (AF s a) where
  pi = approxSingleton pi
  exp = minrange exp exp Convex
  log x
    | inf x > 0  = minrange log recip Concave x
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
newtype AFMT t s m a = AFMT {runAFMT :: s -> m (a, s)}
type AFM t a = AFMT t AFIndex Identity a

instance (Monad m) => Functor (AFMT t s m) where
  fmap = liftM

instance (Monad m) => Applicative (AFMT t s m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (AFMT t s m) where
  return a = AFMT $ \s -> return (a, s)
  (AFMT x) >>= f = AFMT $ \s -> do
    (v, s') <- x s
    (AFMT x') <- return $ f v
    x' s'

instance (Monad m) => MonadState s (AFMT t s m) where
  get   = AFMT $ \s -> return (s, s)
  put s = AFMT $ \_ -> return ((), s)

instance MonadTrans (AFMT t s) where
  lift c = AFMT $ \s -> c >>= (\x -> return (x, s))

-- | This gives an affine form with midpoint 0 and radius 1.
-- This affine form does not share epsilons with any affine forms created before it.
-- It can be used to instantiate new affine forms.
newEps :: Num a => AFM t (AF t a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AF 0 (replicate idx 0 ++ [1]) 0

-- | Creates a new affine form that covers the interval.
-- This affine form does not share epsilons with any affine forms created before it.
newFromInterval :: (Eq a, Fractional a, ExplicitRounding a) => IA.Interval a -> AFM t (AF t a)
newFromInterval i = do
  eps <- newEps
  let mult = ((IA.width i) / 2) .* eps
  return $ (IA.midpoint i) .+ mult

-- | Creates a new affine form that represents some exact value
singleton :: (Num a) => a -> AF s a
singleton x = AF x [] 0

-- | Creates a new affine form that approximately represents some value.
-- This function adds a small error to account for the 'wobble' in the computer representation of the value.
approxSingleton :: (ExplicitRounding a) => a -> AF s a
approxSingleton x = AF x [] $ eps x

-- | Evaluates the AFM monad. It is not possible to get an AF out of an AFM monad.
evalAFM :: forall a b. (forall t. AFM t b) -> b
evalAFM (AFMT x) = fst . runIdentity $ x 0

-- | Gives the radius of the affine form
radius :: (Num a, ExplicitRounding a) => AF s a -> a
radius (AF _ xs xe) = v
  where v = xe +/ (sumup $ abs <$> xs)

-- | Gives the midpoint of the affine form (the first term of the affine form).
midpoint :: AF s a -> a
midpoint (AF x _ _) = x

-- | Gives the minimal possible value of the affine form
inf :: (Num a, ExplicitRounding a) => AF s a -> a
inf af = x - eps x
  where x = (midpoint af) - (radius af)

-- | Gives the maximal possible value of the affine form
sup :: (Num a, ExplicitRounding a) => AF s a -> a
sup af = x + eps x
  where x = (midpoint af) + (radius af)

-- | Gives the corresponding interval of the affine form
interval :: (Num a, Ord a, ExplicitRounding a) => AF s a -> IA.Interval a
interval af = (inf af)...(sup af)

-- | Returns whether the element is representable by the affine form
member :: (Num a, Ord a, ExplicitRounding a) => a -> AF s a -> Bool
member x af = x `IA.member` (interval af)

epscount_ :: AF s a -> Int
epscount_ (AF _ xs _) = length xs

-- Affine arithmetic operations

-- | Sets the midpoint of the affine form
setMidpoint :: (Num a, ExplicitRounding a) => a -> AF s a -> AF s a
setMidpoint m (AF x xs xe) = AF m xs $ xe + eps m

-- | Adds the value to the error term of the affine form
addError :: (Num a, Ord a) => AF s a -> a -> AF s a
addError (AF x xs xe) e
  | e >= 0 = AF x xs (xe + e)
  | otherwise = throw AddingNegativeError

-- | Adds a scalar value to the affine form
(.+) :: (Num a, ExplicitRounding a) => a -> AF s a -> AF s a
a .+ (AF x xs xe) = AF m xs (xe + rnd)
  where m = x + a
        rnd = eps $ a + xe

add :: (ExplicitRounding a, Num a, Ord a) => AF s a -> AF s a -> AF s a
(AF x xs xe) `add` (AF y ys ye) = addError af rnd
  where zs  = (uncurry (+)) <$> embed xs ys
        af  = AF (x + y) zs (xe +/ ye)
        rnd = sumup $ (uncurry (+/)) <$> embed (eps <$> xs ++ [x]) (eps <$> ys ++ [y])

negateAF :: (Num a) => AF s a -> AF s a
negateAF (AF x xs xe) = AF (-x) (negate <$> xs) xe

multiply :: (Num a, Ord a, ExplicitRounding a) => AF s a -> AF s a -> AF s a
af1@(AF x xs xe) `multiply` af2@(AF y ys ye) = addError af rnd
  where zs = uncurry (+) <$> embed ((y*) <$> xs) ((x*) <$> ys)
        ze1 = sum $ liftM2 (*/) (abs <$> xs ++ [xe]) (abs <$> ys ++ [ye])
        ze2 = (abs x */ ye) +/ (abs y */ xe)
        af = AF (x * y) zs (ze1 +/ ze2)
        -- fig-sto-97:74
        rnd = sumup $ (uncurry (*/)) <$> liftM2 (,) (eps <$> xs ++ [x, xe]) (eps <$> ys ++ [y, ye])

-- | Multiplies the affine form by a scalar
(.*) :: (Eq a, Num a, Ord a, ExplicitRounding a) => a -> AF s a -> AF s a
a .* (AF x xs xe) = addError af rnd
  where af = AF (a*x) ((a*) <$> xs) $ (a * xe)
        rnd = sumup $ eps . (a */) <$> xs ++ [x, xe]

recipAF :: (Ord a, Fractional a, ExplicitRounding a) => AF s a -> AF s a
recipAF af
  | low > 0   = minrange recip (\x -> -1/x^2) Convex af
  | high < 0  = negateAF . recipAF $ negateAF af
  | otherwise = throw DivisionByZero
  where high = sup af
        low  = inf af

cosAF :: (Ord a, RealFrac a, Floating a, ExplicitRounding a) => AF s a -> AF s a
cosAF af
  | radius af < pi = f af
  | otherwise = AF 0 [] 1
  where a = inf af `pmod'` (2*pi)
        b = sup af `pmod'` (2*pi)
        f x
          -- function never reaches extremum
          | a < pi && b < pi || a > pi && b > pi = minrange cos (negate . sin) undefined af
          -- function reaches extremum exactly once
          | a < b = AF (rl - 1) [] rl
          -- function reaches extremum more than once
          | otherwise = AF (1 - rh) [] rh
          where rl = abs (1 + (max (cos a) (cos b)))/2
                rh = abs (1 - (min (cos a) (cos b)))/2

sinAF :: (Ord a, RealFrac a, Floating a, ExplicitRounding a) => AF s a -> AF s a
sinAF af = cosAF ((-pi/2) .+ af)

absAF :: (Ord a, ExplicitRounding a, Fractional a) => AF s a -> AF s a
absAF af
  | inf af >= 0 = af
  | sup af <= 0 = -af
  | otherwise = AF x [] x
    where x = (max (abs . sup $ af) (abs . inf $ af))/2

signumAF :: (Ord a, Num a, ExplicitRounding a) => AF s a -> AF s a
signumAF af
  | inf af >= 0 = AF 1 [] 0
  | sup af <= 0 = AF (-1) [] 0
  | otherwise = AF 0 [] 1

--
-- Helper functions
--

-- | Fixes the epsilons of the affine form to the values in the list.
-- The list will be padded with zeroes to match the number of coefficients.
fix :: (Num a, Ord a, ExplicitRounding a) => AF s a -> [a] -> IA.Interval a
fix (AF x xs xe) vals = (l)...(h)
  where em = embed xs vals
        s = sum $ uncurry (*) <$> em
        m = x + s
        l = m - xe
        h = m + xe

-- | Returns a min-range approximation function for given function and its derivative.
minrange :: (Fractional a, Ord a, ExplicitRounding a) => (a -> a) -> (a -> a) -> Curvature -> (AF s a -> AF s a)
minrange f f' curv = \af ->
  let a   = sup af
      b   = inf af
      p   = case curv of
              Convex  -> f' a
              Concave -> f' b
      q   = ((f a)+(f b)-p*(a+b))/2
      d   = abs ((f a)-(f b)+p*(a-b))/2
      rnd = eps $ (eps $ q +/ a */ p) + (eps $ q +/ b */ p)
      af1 = q .+ (p .* af) `addError` (d + rnd)
  in
    addError af1 rnd
