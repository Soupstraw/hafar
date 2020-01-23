{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Control.Monad
import System.Random
import Text.Printf

import qualified Numeric.Interval as IA (member, inf, sup, contains, inflate)
import Numeric.AffineForm

data Approx a = Approx a (AF a)

data EpsV a = EpsV [a]
  deriving (Show)

newtype ZerolessAF a = ZerolessAF (AF a)
  deriving (Show)

instance (Fractional a, Ord a, Arbitrary a) => Arbitrary (ZerolessAF a) where
  arbitrary = do
    af <- arbitrary
    let mh = max (midpoint af) (range af)
        ml = min (midpoint af) (negate $ range af)
    if midpoint af >= 0
       then return . ZerolessAF $ setMidpoint mh af
       else return . ZerolessAF $ setMidpoint ml af

newtype PositiveAF a = PositiveAF (AF a)
  deriving (Show)

instance (Fractional a, Ord a, Arbitrary a) => Arbitrary (PositiveAF a) where
  arbitrary = do
    af <- arbitrary
    let m = max (midpoint af) (range af)
    return . PositiveAF $ setMidpoint m af

instance (Real a, Arbitrary a, Random a) => Arbitrary (EpsV a) where
  arbitrary = do
    l <- listOf $ choose (-1, 1)
    return $ EpsV l
  shrink (EpsV l) = filter validEV $ EpsV <$> (shrink l)

valid :: (Ord a, Num a) => Approx a -> Bool
valid (Approx x af) = x `IA.member` i
  where i = interval af

validEV :: (Ord a, Num a) => EpsV a -> Bool
validEV (EpsV l) = all (\x -> -1 <= x && x <= 1) l

instance (Ord a, Num a, Arbitrary a, Random a) => Arbitrary (Approx a) where
  arbitrary = do
    af <- arbitrary
    let i = interval af
    x <- choose (IA.inf i, IA.sup i)
    return $ Approx x af
  shrink (Approx x af) = filter valid [Approx x' af' | (x', af') <- shrink (x, af)]

-- RuKaS14.pdf [1102:2]
prop_addition :: EpsV Double -> AF Double -> AF Double -> Bool
prop_addition (EpsV e) x y = x `fix` e + y `fix` e == (x + y) `fix` e

prop_subtraction :: EpsV Double -> AF Double -> AF Double -> Bool
prop_subtraction (EpsV e) x y = x `fix` e - y `fix` e == (x - y) `fix` e

prop_multiplication :: EpsV Double -> AF Double -> AF Double -> Property
prop_multiplication (EpsV e) x y = counterexample str res
  where lhs = (x * y) `fix` e
        rhs = x `fix` e * y `fix` e
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_power :: EpsV Double -> AF Double -> Small Int -> Property
prop_power (EpsV e) x y = counterexample str res
  where n = (abs $ getSmall y) `mod` 3
        lhs = (x ^ n) `fix` e
        rhs = (x `fix` e) ^ n
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_recip :: EpsV Double -> ZerolessAF Double -> Property
prop_recip (EpsV e) (ZerolessAF x) = counterexample str $ property res
  where lhs = (1/x) `fix` e
        rhs = 1/(x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_log :: EpsV Double -> PositiveAF Double -> Property
prop_log (EpsV e) (PositiveAF x) = counterexample str $ property res
  where lhs = (log x) `fix` e
        rhs = log (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_exp :: EpsV Double -> AF Double -> Property
prop_exp (EpsV e) x = counterexample str $ property res
  where lhs = (exp x) `fix` e
        rhs = exp (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_cos :: EpsV Double -> AF Double -> Property
prop_cos (EpsV e) x = counterexample str $ property res
  where lhs = (cos x) `fix` e
        rhs = cos (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_sin :: EpsV Double -> AF Double -> Property
prop_sin (EpsV e) x = counterexample str $ property res
  where lhs = (1e-15) `IA.inflate` ((sin x) `fix` e) -- Inflating the interval by a little because of a bug in the intervals library
        rhs = sin (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_tan :: EpsV Double -> AF Double -> Property
prop_tan (EpsV e) x = counterexample str $ property res
  where lhs = (tan x) `fix` e
        rhs = tan $ (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

-- Testing boilerplate

return []

main :: IO ()
main = do
  $quickCheckAll
  return ()

