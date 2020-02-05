{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck

import Control.Monad
import System.Random
import Text.Printf
import Data.Fixed (mod')

import qualified Numeric.Interval as IA (member, inf, sup, contains, inflate, Interval, midpoint)
import Numeric.AffineForm
import Numeric.AffineForm.Utils

--
-- Generators and modifiers
--

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

newtype ZerolessAF s a = ZerolessAF (AF s a)

instance (Show a) => Show (ZerolessAF s a) where
  show (ZerolessAF x) = show x

instance (Fractional a, Ord a, Arbitrary a, ExplicitRounding a) => Arbitrary (ZerolessAF s a) where
  arbitrary = do
    af <- arbitrary
    let mh = max (midpoint af) (1 + radius af)
        ml = min (midpoint af) ((negate $ radius af) - 1)
        res
          | midpoint af >= 0 = ZerolessAF $ setMidpoint mh af
          | otherwise = ZerolessAF $ setMidpoint ml af
    return res

newtype PositiveAF s a = PositiveAF (AF s a)

instance (Show a) => Show (PositiveAF s a) where
  show (PositiveAF x) = show x

instance (Fractional a, Ord a, Arbitrary a, ExplicitRounding a) => Arbitrary (PositiveAF s a) where
  arbitrary = do
    af <- arbitrary
    let m = 1/100000 + max (midpoint af) (radius af)
    return . PositiveAF $ setMidpoint m af

newtype SmallAF s a = SmallAF (AF s a)

instance (Show a) => Show (SmallAF s a) where
  show (SmallAF x) = show x

instance (Floating a, Ord a, Arbitrary a, ExplicitRounding a) => Arbitrary (SmallAF s a) where
  arbitrary = do
    size <- getSize
    af <- arbitrary
    let s = log . fromIntegral $ size + 1
        k = s / (radius af)
        m = clamp (midpoint af) (-s) s
    return . SmallAF $ setMidpoint m (k .* af)

validEV :: (Ord a, Num a) => EpsV a -> Bool
validEV (EpsV l) = all (\x -> -1 <= x && x <= 1) l

--
-- Properties
--

-- Generalized

correctnessPropUnary :: (Fractional a, Ord a, Show a, ExplicitRounding a) =>
  (AF s a -> AF s a) ->
  (a -> a) ->
  [a] ->
  AF s a ->
  Property
correctnessPropUnary f g e x = counterexample str res
  where lhs = (f x) `fix` e
        rhs = g (IA.midpoint $ fix x e)
        res = rhs `IA.member` lhs
        str = "AA: " ++ (show lhs) ++ "\n"
           ++ "IA: " ++ (show rhs)

correctnessPropBinary :: (Fractional a, Ord a, Show a, ExplicitRounding a) =>
  (AF s a -> AF s a -> AF s a) ->
  (a -> a -> a) ->
  [a] ->
  AF s a ->
  AF s a ->
  Property
correctnessPropBinary f g e x y = counterexample str res
  where lhs = (f x y) `fix` e
        rhs = g (IA.midpoint $ fix x e) (IA.midpoint $ fix y e)
        res = rhs `IA.member` lhs
        str = "AA: " ++ (show lhs) ++ "\n"
           ++ "IA: " ++ (show rhs)

-- RuKaS14.pdf [1102:2]
-- prop_addition :: EpsV Double -> AF Double -> AF Double -> Property
-- prop_addition (EpsV e) x y = counterexample str res
--   where lhs = (x + y) `fix` e
--         rhs = x `fix` e + y `fix` e
--         res = lhs `IA.contains` rhs
--         str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_addition :: EpsV Double -> AF s Double -> AF s Double -> Property
prop_addition (EpsV e) x y = correctnessPropBinary (+) (+) e x y

prop_subtraction :: EpsV Double -> AF s Double -> AF s Double -> Property
prop_subtraction (EpsV e) x y = correctnessPropBinary (-) (-) e x y

prop_multiplication :: EpsV Double -> AF s Double -> AF s Double -> Property
prop_multiplication (EpsV e) x y = correctnessPropBinary (*) (*) e x y

prop_power :: EpsV Double -> AF s Double -> SmallExponent Integer -> Property
prop_power (EpsV e) x (SmallExponent n) = correctnessPropUnary (^n) (^n) e x

prop_recip :: EpsV Double -> ZerolessAF s Double -> Property
prop_recip (EpsV e) (ZerolessAF x) = correctnessPropUnary recip recip e x

prop_log :: EpsV Double -> PositiveAF s Double -> Property
prop_log (EpsV e) (PositiveAF x) = correctnessPropUnary log log e x

prop_exp :: EpsV Double -> SmallAF s Double -> Property
prop_exp (EpsV e) (SmallAF x) = correctnessPropUnary exp exp e x

prop_abs :: EpsV Double -> AF s Double -> Property
prop_abs (EpsV e) x = correctnessPropUnary abs abs e x

--
-- Testing boilerplate
--

return [] -- This is a hack to make the quickCheckAll template work correctly

main :: IO ()
main = do
  $quickCheckAll
  return ()

