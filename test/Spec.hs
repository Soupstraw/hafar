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
prop_addition :: EpsV Double -> AF Double -> AF Double -> Property
prop_addition (EpsV e) x y = counterexample str res
  where lhs = IA.inflate tinyFloat $ (x + y) `fix` e
        rhs = x `fix` e + y `fix` e
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_subtraction :: EpsV Double -> AF Double -> AF Double -> Property
prop_subtraction (EpsV e) x y = counterexample str res
  where lhs = IA.inflate tinyFloat $ (x - y) `fix` e
        rhs = x `fix` e - y `fix` e
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_multiplication :: EpsV Double -> AF Double -> AF Double -> Property
prop_multiplication (EpsV e) x y = counterexample str res
  where lhs = IA.inflate tinyFloat $ (x * y) `fix` e
        rhs = x `fix` e * y `fix` e
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_power :: EpsV Double -> AF Double -> Small Int -> Property
prop_power (EpsV e) x y = counterexample str res
  where n = (abs $ getSmall y) `mod` 3
        lhs = IA.inflate tinyFloat $ (x ^ n) `fix` e
        rhs = (x `fix` e) ^ n
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_recip :: EpsV Double -> ZerolessAF Double -> Property
prop_recip (EpsV e) (ZerolessAF x) = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (recip x) `fix` e
        rhs = recip (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_log :: EpsV Double -> PositiveAF Double -> Property
prop_log (EpsV e) (PositiveAF x) = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (log x) `fix` e
        rhs = log (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_exp :: EpsV Double -> AF Double -> Property
prop_exp (EpsV e) x = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (exp x) `fix` e
        rhs = exp (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_cos :: EpsV Double -> AF Double -> Property
prop_cos (EpsV e) x = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (cos x) `fix` e
        rhs = cos (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_sin :: EpsV Double -> AF Double -> Property
prop_sin (EpsV e) x = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (sin x) `fix` e
        rhs = sin (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

prop_tan :: EpsV Double -> AF Double -> Property
prop_tan (EpsV e) x = counterexample str $ property res
  where lhs = IA.inflate tinyFloat $ (tan x) `fix` e
        rhs = tan $ (x `fix` e)
        res = lhs `IA.contains` rhs
        str = (show lhs) ++ "\n" ++ (show rhs)

-- Testing boilerplate

tinyFloat = 1e-10

return [] -- This is a hack to make the quickCheckAll template work correctly

main :: IO ()
main = do
  $quickCheckAll
  return ()

