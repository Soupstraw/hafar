{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

import Control.Monad
import System.Random
import Text.Printf
import Data.Fixed (mod')

import qualified Numeric.Interval as IA (member, inf, sup, contains, inflate)
import Numeric.AffineForm
import Numeric.AffineForm.Utils

--
-- Generators and modifiers
--

data Approx a = Approx a (AF a)

data EpsV a = EpsV [a]
  deriving (Show)

instance (Real a, Arbitrary a) => Arbitrary (EpsV a) where
  arbitrary = do
    l <- listOf $ arbitrary
    let ls = (\x -> (x `mod'` 2) -1) <$> l
    return $ EpsV ls
  shrink (EpsV l) = filter validEV $ EpsV <$> (shrink l)

newtype SmallExponent a = SmallExponent a
  deriving (Show)

instance (Enum a, Num a, Arbitrary a) => Arbitrary (SmallExponent a) where
  arbitrary = SmallExponent <$> elements [1..4]
  shrink (SmallExponent x) = SmallExponent <$> shrink x

newtype ZerolessAF a = ZerolessAF (AF a)
  deriving (Show)

instance (Fractional a, Ord a, Arbitrary a) => Arbitrary (ZerolessAF a) where
  arbitrary = do
    af <- arbitrary
    let mh = max (midpoint af) (1 + range af)
        ml = min (midpoint af) ((negate $ range af) - 1)
        res
          | midpoint af >= 0 = ZerolessAF $ setMidpoint mh af
          | otherwise = ZerolessAF $ setMidpoint ml af
    return res

newtype PositiveAF a = PositiveAF (AF a)
  deriving (Show)

instance (Fractional a, Ord a, Arbitrary a) => Arbitrary (PositiveAF a) where
  arbitrary = do
    af <- arbitrary
    let m = 1/100000 + max (midpoint af) (range af)
    return . PositiveAF $ setMidpoint m af

newtype SmallAF a = SmallAF (AF a)
  deriving (Show)

instance (Floating a, Ord a, Arbitrary a) => Arbitrary (SmallAF a) where
  arbitrary = do
    size <- getSize
    af <- arbitrary
    let s = log . fromIntegral $ size + 1
        k = s / (range af)
        m = clamp (midpoint af) (-s) s
    return . SmallAF $ setMidpoint m (k .* af)

instance (Ord a, Num a, Arbitrary a, Random a) => Arbitrary (Approx a) where
  arbitrary = do
    af <- arbitrary
    let i = interval af
    x <- choose (IA.inf i, IA.sup i)
    return $ Approx x af
  shrink (Approx x af) = filter valid [Approx x' af' | (x', af') <- shrink (x, af)]

valid :: (Ord a, Num a) => Approx a -> Bool
valid (Approx x af) = x `IA.member` i
  where i = interval af

validEV :: (Ord a, Num a) => EpsV a -> Bool
validEV (EpsV l) = all (\x -> -1 <= x && x <= 1) l

--
-- Properties
--

-- RuKaS14.pdf [1102:2]
prop_addition :: EpsV Rational -> AF Rational -> AF Rational -> Property
prop_addition (EpsV e) x y = counterexample str res
  where lhs = (x + y) `fix` e
        rhs = x `fix` e + y `fix` e
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_subtraction :: EpsV Rational -> AF Rational -> AF Rational -> Property
prop_subtraction (EpsV e) x y = counterexample str res
  where lhs = (x - y) `fix` e
        rhs = x `fix` e - y `fix` e
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_multiplication :: EpsV Rational -> AF Rational -> AF Rational -> Property
prop_multiplication (EpsV e) x y = counterexample str res
  where lhs = (x * y) `fix` e
        rhs = x `fix` e * y `fix` e
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_power :: EpsV Rational -> AF Rational -> SmallExponent Integer -> Property
prop_power (EpsV e) x (SmallExponent n) = counterexample str res
  where lhs = (x ^ n) `fix` e
        rhs = (x `fix` e) ^ n
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_recip :: EpsV Rational -> ZerolessAF Rational -> Property
prop_recip (EpsV e) (ZerolessAF x) = counterexample str $ property res
  where lhs = (recip x) `fix` e
        rhs = recip (x `fix` e)
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_log :: EpsV Double -> PositiveAF Double -> Property
prop_log (EpsV e) (PositiveAF x) = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (log x) `fix` e
        rhs = log (x `fix` e)
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_exp :: EpsV Double -> SmallAF Double -> Property
prop_exp (EpsV e) (SmallAF x) = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (exp x) `fix` e
        rhs = exp (x `fix` e)
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

-- Disabled these tests for now since trig seems to be broken in the intervals library

-- prop_cos :: EpsV Double -> AF Double -> Property
-- prop_cos (EpsV e) x = counterexample str $ property res
--   where lhs = IA.inflate tinyFloat $ (cos x) `fix` e
--         rhs = cos (x `fix` e)
--         res = lhs `IA.contains` rhs
--         str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

-- prop_sin :: EpsV Double -> AF Double -> Property
-- prop_sin (EpsV e) x = counterexample str $ property res
--   where lhs = IA.inflate tinyFloat $ (sin x) `fix` e
--         rhs = sin (x `fix` e)
--         res = lhs `IA.contains` rhs
--         str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_abs :: EpsV Double -> AF Double -> Property
prop_abs (EpsV e) x = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (abs x) `fix` e
        rhs = abs $ x `fix` e
        res = lhs `IA.contains` rhs
        str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

--
-- Testing boilerplate
--

tinyFloat = 1e-10
tinyRational = 1/1000000000 :: Rational

return [] -- This is a hack to make the quickCheckAll template work correctly

main :: IO ()
main = do
  $quickCheckAll
  return ()

