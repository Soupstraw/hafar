module AffineForm where

import Control.Monad.State

import qualified Numeric.Interval as IA
import Numeric.Interval ((...))

data AffineForm a
  = AffineForm a [a] a
  | Reals
  deriving (Show)

type AFIndex = Int
type AFM = State AFIndex

newState :: AFM ()
newState = put 0

eval :: AFM a -> a
eval x = fst $ runState x 0

newEps :: Num a => AFM (AffineForm a)
newEps = do
  idx <- get
  put $ idx + 1
  return $ AffineForm 0 (replicate idx 0 ++ [1]) 0

newFromInterval :: Fractional a => IA.Interval a -> AFM (AffineForm a)
newFromInterval i = do
  eps <- newEps
  mult <- ((IA.width i) / 2) *^ eps
  (IA.midpoint i) +^ mult

-- Returns range without accounting for the error term
range_ :: (Num a, Monad m) => AffineForm a -> m a
range_ (AffineForm _ xs _) = return . sum $ abs <$> xs

range :: (Num a, Monad m) => AffineForm a -> m a
range af@(AffineForm _ _ xe) = (xe+) <$> range_ af

midpoint :: (Num a, Monad m) => AffineForm a -> m a
midpoint (AffineForm x _ _) = return x
midpoint Reals = error "Reals has no midpoint"

lo :: (Num a, Monad m) => AffineForm a -> m a
lo af = do
  r <- range af
  c <- midpoint af
  return $ r-c

hi :: (Num a, Monad m) => AffineForm a -> m a
hi af = do
  r <- range af
  c <- midpoint af
  return $ r+c

interval :: (Num a, Ord a, Monad m) => AffineForm a -> m (IA.Interval a)
interval af = do
  a <- lo af
  b <- hi af
  return (a...b)

-- Affine arithmetic operations

-- Add scalar
(+^) :: (Num a, Monad m) => a -> AffineForm a -> m (AffineForm a)
a +^ (AffineForm x xs xe) = return $ AffineForm (x+a) xs xe
_ +^ Reals = return Reals

-- Add scalar to error term
(^+~) :: (Num a, Monad m) => AffineForm a -> a -> m (AffineForm a)
(AffineForm x xs xe) ^+~ e = return $ AffineForm x xs (xe+e)
Reals ^+~ _ = return Reals

(^+^) :: (Num a, Monad m) => AffineForm a -> AffineForm a -> m (AffineForm a)
(AffineForm x xs xe) ^+^ (AffineForm y ys ye) = return $ AffineForm (x+y) zs (xe+ye)
  where zs = (\(l,r) -> l+r) <$> zipWithZeros xs ys

negateAF :: (Num a, Monad m) => AffineForm a -> m (AffineForm a)
negateAF (AffineForm x xs xe) = return $ AffineForm (-x) (negate <$> xs) xe

(^-^) :: (Num a, Monad m) => AffineForm a -> AffineForm a -> m (AffineForm a)
x ^-^ y = negateAF y >>= (x ^+^)

(^*^) :: (Num a, Monad m) => AffineForm a -> AffineForm a -> m (AffineForm a)
(AffineForm x xs xe) ^*^ (AffineForm y ys ye) = return $ AffineForm (x*y) zs ze
  where zs = (\(l,r) -> l+r) <$> zipWithZeros ((y*) <$> xs) ((x*) <$> ys)
        ze = sum $ liftM2 (*) (xs ++ [xe]) (ys ++ [ye])

(*^) :: (Num a, Monad m) => a -> AffineForm a -> m (AffineForm a)
a *^ (AffineForm x xs xe) = return $ AffineForm (a*x) ((a*) <$> xs) xe

recipAF :: (Ord a, Fractional a, Monad m) => AffineForm a -> m (AffineForm a)
recipAF af = do
  high <- hi af
  low  <- lo af
  -- Any way to get rid of the if-else statements?
  if low > 0
    then recipAF' af
    else if high < 0
      then negateAF =<< recipAF' =<< negateAF af
      else return Reals

recipAF' :: (Ord a, Fractional a, Monad m) => AffineForm a -> m (AffineForm a)
recipAF' af = do
  a <- hi af
  b <- lo af
  let p = -1/b^2
      q = -p*(a+b)^2/(2*a)
      d = -p*(a-b)^2/(2*a)
  mult <- p *^ af
  shft <- q +^ mult
  shft ^+~ d

(^/^) :: (Ord a, Fractional a, Monad m) => AffineForm a -> AffineForm a -> m (AffineForm a)
x ^/^ y = (x ^*^) =<< (recipAF y)

(^**) :: (Num a, Monad m) => AffineForm a -> a -> m (AffineForm a)
(AffineForm x xs xe) ^** y = undefined

-- Helper functions

zipWithZeros :: (Num a, Num b) => [a] -> [b] -> [(a,b)]
zipWithZeros x y = take (max (length x) (length y)) $ zip infx infy
  where infx = x ++ repeat 0
        infy = y ++ repeat 0

test :: IO ()
test = do
  let comp = do
        xe <- newFromInterval (1...1)
        ye <- newFromInterval (1...3)
        op <- xe ^/^ ye
        i <- interval op
        return $ show i
  undefined

