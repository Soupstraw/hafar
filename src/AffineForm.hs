module AffineForm where

import Control.Monad.State

import Numeric.Interval

data AffineForm a =
  AffineForm a [a] a

type AFIndex = Int
type AFM = State AFIndex

newState :: AFM ()
newState = put 0

newEps :: Num a => AFM (AffineForm a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AffineForm 0 (replicate idx 0 ++ [1]) 0

-- Affine arithmetic operations

(^+^) :: Num a => AffineForm a -> AffineForm a -> AFM (AffineForm a)
(AffineForm x xs xe) ^+^ (AffineForm y ys ye) = return $ AffineForm (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> zipWithZeros xs ys

negateAF :: Num a => AffineForm a -> AFM (AffineForm a)
negateAF (AffineForm x xs xe) = return $ AffineForm (-x) (negate <$> xs) xe

(^-^) :: Num a => AffineForm a -> AffineForm a -> AFM (AffineForm a)
x ^-^ y = negateAF y >>= (x ^+^)

(^*^) :: Num a => AffineForm a -> AffineForm a -> AFM (AffineForm a)
(AffineForm x xs xe) ^*^ (AffineForm y ys ye) = return $ AffineForm (x*y) zs ze
  where zs = (\(l,r) -> l+r) <$> zipWithZeros ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) xs ys

recipAF :: Num a => AffineForm a -> AffineForm a -> AFM (AffineForm a)
recipAF (AffineForm y ys ye) = undefined

(^/^) :: Num a => AffineForm a -> AffineForm a -> AFM (AffineForm a)
(AffineForm x xs xe) ^/^ (AffineForm y ys ye) = undefined

(^**^) :: Num a => AffineForm a -> a -> AFM (AffineForm a)
(AffineForm x xs xe) ^**^ y = undefined

-- Helper functions

zipWithZeros :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
zipWithZeros x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0
