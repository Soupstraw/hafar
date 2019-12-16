module AffineForm (newEps,
                   newFromInterval,
                   range,
                   midpoint,
                   lo, hi,
                   interval,
                   negateAF,
                   recipAF,
                   (+^), (^+~),
                   (^-^),
                   (^*^), (*^),
                   (^/^), (^**),
                   log, min, max,
                   abs, sin, cos,
                   tan
                  ) where

import Control.Monad.State

import qualified Numeric.Interval as IA
import Numeric.Interval ((...))

data AffineForm a
  = AffineForm a [a] a
  | Reals
  deriving (Show)

type AFIndex = Int
type AFM = State AFIndex

newEps :: Num a => AFM (AffineForm a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AffineForm 0 (replicate idx 0 ++ [1]) 0

newFromInterval :: Fractional a => IA.Interval a -> AFM (AffineForm a)
newFromInterval i = do
  eps <- newEps
  let mult = ((IA.width i) / 2) *^ eps
  return $ (IA.midpoint i) +^ mult

-- Returns range without accounting for the error term
range_ :: (Num a) => AffineForm a -> a
range_ (AffineForm _ xs _) = sum $ abs <$> xs

range :: (Num a) => AffineForm a -> a
range af@(AffineForm _ _ xe) = xe + range_ af

midpoint :: (Num a) => AffineForm a -> a
midpoint (AffineForm x _ _) = x
midpoint Reals = error "Reals has no midpoint"

lo :: (Num a) => AffineForm a -> a
lo af = (midpoint af) - (range af)

hi :: (Num a) => AffineForm a -> a
hi af = (midpoint af) + (range af)

interval :: (Num a, Ord a) => AffineForm a -> IA.Interval a
interval af = (lo af)...(hi af)

-- Affine arithmetic operations

-- Add scalar
(+^) :: (Num a) => a -> AffineForm a -> AffineForm a
a +^ (AffineForm x xs xe) = AffineForm (x+a) xs xe
_ +^ Reals = Reals

-- Add scalar to error term
(^+~) :: (Num a) => AffineForm a -> a -> AffineForm a
(AffineForm x xs xe) ^+~ e = AffineForm x xs (xe+e)
Reals ^+~ _ = Reals

(^+^) :: (Num a) => AffineForm a -> AffineForm a -> AffineForm a
(AffineForm x xs xe) ^+^ (AffineForm y ys ye) = AffineForm (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> zipWithZeros xs ys

negateAF :: (Num a) => AffineForm a -> AffineForm a
negateAF (AffineForm x xs xe) = AffineForm (-x) (negate <$> xs) xe

(^-^) :: (Num a) => AffineForm a -> AffineForm a -> AffineForm a
x ^-^ y = x ^+^ (negateAF y)

(^*^) :: (Num a) => AffineForm a -> AffineForm a -> AffineForm a
(AffineForm x xs xe) ^*^ (AffineForm y ys ye) = AffineForm (x*y) zs ze
  where zs = (\(l,r) -> l+r) <$> zipWithZeros ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) (xs ++ [xe]) (ys ++ [ye])

(*^) :: (Num a) => a -> AffineForm a -> AffineForm a
a *^ (AffineForm x xs xe) = AffineForm (a*x) ((a*) <$> xs) xe

recipAF :: (Ord a, Fractional a) => AffineForm a -> AffineForm a
recipAF af = do
  let high = hi af
  let low  = lo af
  -- Any way to get rid of the if-else statements?
  if low > 0
    then recipAF' af
    else if high < 0
      then negateAF . recipAF $ negateAF af
      else Reals

recipAF' :: (Ord a, Fractional a) => AffineForm a -> AffineForm a
recipAF' af =
  let a = hi af
      b = lo af
      p = -1/b^2
      q = -p*(a+b)^2/(2*a)
      d = -p*(a-b)^2/(2*a)
  in q +^ (p *^ af) ^+~ d

(^/^) :: (Ord a, Fractional a) => AffineForm a -> AffineForm a -> AffineForm a
x ^/^ y = x ^*^ (recipAF y)

(^**) :: (Num a) => AffineForm a -> a -> AffineForm a
(AffineForm x xs xe) ^** y = undefined

-- Helper functions

zipWithZeros :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
zipWithZeros x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0

