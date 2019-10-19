module AffineForm where

import Control.Monad.State

import Numeric.Interval hiding (midpoint)

data AffineForm a =
  AffineForm a [a] a
  deriving (Show)

type AFIndex = Int
type AFM = State AFIndex

newState :: AFM ()
newState = put 0

newEps :: Num a => AFM (AffineForm a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AffineForm 0 (replicate idx 0 ++ [1]) 0

-- Returns range without accounting for the error term
range_ :: Num a => AffineForm a -> AFM a
range_ (AffineForm _ xs _) = return . sum $ abs <$> xs

range :: Num a => AffineForm a -> AFM a
range af@(AffineForm _ _ xe) = (xe+) <$> range_ af

midpoint :: Num a => AffineForm a -> AFM a
midpoint (AffineForm x _ _) = return x

lo :: Num a => AffineForm a -> AFM a
lo af = do
  r <- range af
  c <- midpoint af
  return $ r-c

hi :: Num a => AffineForm a -> AFM a
hi af = do
  r <- range af
  c <- midpoint af
  return $ r+c

-- Affine arithmetic operations

-- Add scalar to midpoint
(^+.) :: Num a => AffineForm a -> a -> AFM (AffineForm a)
(AffineForm x xs xe) ^+. a = return $ AffineForm (x+a) xs xe

-- Add scalar to error term
(^+~) :: Num a => AffineForm a -> a -> AFM (AffineForm a)
(AffineForm x xs xe) ^+~ e = return $ AffineForm x xs (xe+e)

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
        ze = sum $ liftM2 (*) (xs ++ [xe]) (ys ++ [ye])

(*^) :: Num a => a -> AffineForm a -> AFM (AffineForm a)
a *^ (AffineForm x xs xe) = return $ AffineForm (a*x) ((a*) <$> xs) xe

recipAF :: Fractional a => AffineForm a -> AFM (AffineForm a)
recipAF af = do
  high <- hi af
  low <- lo af
  let p = -1/high^^2
      q = -p*(low+high)^^2/(2*low)
      delta = -p*(low-high)^^2/(2*low)
  scl <- p *^ af
  shft <- scl ^+. q
  shft ^+~ delta

(^/^) :: Fractional a => AffineForm a -> AffineForm a -> AFM (AffineForm a)
x ^/^ y = (x ^*^) =<< (recipAF y)

(^**^) :: Num a => AffineForm a -> a -> AFM (AffineForm a)
(AffineForm x xs xe) ^**^ y = undefined

-- Helper functions

zipWithZeros :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
zipWithZeros x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0
