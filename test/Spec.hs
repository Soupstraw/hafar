{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck

import Control.Monad
import Text.Printf
import Data.Fixed (mod')

import qualified Numeric.Interval as IA (member, inf, sup, contains, inflate, Interval, midpoint)
import Numeric.AffineForm.Internal
import Numeric.AffineForm.Utils
import Numeric.AffineForm.ExplicitRounding

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

instance (Num a, Ord a, Arbitrary a) => Arbitrary (AF s a) where
  arbitrary = do
                x <- arbitrary
                xs <- arbitrary
                (Positive xe) <- arbitrary
                return $ AF x xs xe
  shrink (AF x xs xe) =
    [AF x' xs' xe' | (x', xs', xe') <- shrink (x, xs, xe)]

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

correctnessPropUnary :: (Fractional a, Ord a, Show a, ExplicitRounding a)
  => (AF s a -> AF s a)
  -> (a -> a)
  -> [a]
  -> AF s a
  -> Property
correctnessPropUnary f g e x = withMaxSuccess 5000 $ counterexample str res
  where af = f x
        rhs = g (IA.midpoint $ fix x e)
        rhs_lo = g (IA.inf $ fix x e)
        rhs_hi = g (IA.sup $ fix x e)
        res = rhs `IA.member` interval af .&&.
              rhs_lo `IA.member` interval af .&&.
              rhs_hi `IA.member` interval af
        str = "-- RESULTS --\n"
           ++ "- LHS -\n"
           ++ "AF: " ++ (show af) ++ "\n"
           ++ "INTERVAL: " ++ (show $ interval af) ++ "\n"
           ++ "- RHS -\n"
           ++ "MID: " ++ (show rhs) ++ "\n"
           ++ "HI: " ++ (show rhs_hi) ++ "\n"
           ++ "LO: " ++ (show rhs_lo) ++ "\n"

correctnessPropBinary :: (Fractional a, Ord a, Show a, ExplicitRounding a)
  => (AF s a -> AF s a -> AF s a)
  -> (a -> a -> a)
  -> [a]
  -> AF s a
  -> AF s a
  -> Property
correctnessPropBinary f g e x y = withMaxSuccess 5000 $ counterexample str res
  where af = f x y
        rhs = g (IA.midpoint $ fix x e) (IA.midpoint $ fix y e)
        rhs_lo = g (IA.inf $ fix x e) (IA.inf $ fix y e)
        rhs_hi = g (IA.sup $ fix x e) (IA.sup $ fix y e)

        res = rhs `IA.member` interval af .&&.
              rhs_lo `IA.member` interval af .&&.
              rhs_hi `IA.member` interval af
        str = "-- RESULTS --\n"
           ++ "- LHS -\n"
           ++ "AF: " ++ (show af) ++ "\n"
           ++ "INTERVAL: " ++ (show $ interval af) ++ "\n"
           ++ "- RHS -\n"
           ++ "MID: " ++ (show rhs) ++ "\n"
           ++ "HI: " ++ (show rhs_hi) ++ "\n"
           ++ "LO: " ++ (show rhs_lo) ++ "\n"

-- RuKaS14.pdf [1102:2]
-- prop_addition :: EpsV Double -> AF Double -> AF Double -> Property
-- prop_addition (EpsV e) x y = counterexample str res
--   where lhs = (x + y) `fix` e
--         rhs = x `fix` e + y `fix` e
--         res = lhs `IA.contains` rhs
--         str = "AA: " ++ (show lhs) ++ "\nIA: " ++ (show rhs)

prop_sound_addition :: EpsV Double -> AF s Double -> AF s Double -> Property
prop_sound_addition (EpsV e) x y = correctnessPropBinary (+) (+) e x y

prop_sound_subtraction :: EpsV Double -> AF s Double -> AF s Double -> Property
prop_sound_subtraction (EpsV e) x y = correctnessPropBinary (-) (-) e x y

prop_sound_multiplication :: EpsV Double -> AF s Double -> AF s Double -> Property
prop_sound_multiplication (EpsV e) x y = correctnessPropBinary (*) (*) e x y

prop_sound_power :: EpsV Double -> AF s Double -> SmallExponent Integer -> Property
prop_sound_power (EpsV e) x (SmallExponent n) = correctnessPropUnary (^n) (^n) e x

prop_sound_recip :: EpsV Double -> ZerolessAF s Double -> Property
prop_sound_recip (EpsV e) (ZerolessAF x) = correctnessPropUnary recip recip e x

prop_sound_log :: EpsV Double -> PositiveAF s Double -> Property
prop_sound_log (EpsV e) (PositiveAF x) = correctnessPropUnary log log e x

prop_sound_exp :: EpsV Double -> SmallAF s Double -> Property
prop_sound_exp (EpsV e) (SmallAF x) = correctnessPropUnary exp exp e x

prop_sound_abs :: EpsV Double -> AF s Double -> Property
prop_sound_abs (EpsV e) x = correctnessPropUnary abs abs e x

-- prop_sin :: EpsV Double -> AF s Double -> Property
-- prop_sin (EpsV e) x = correctnessPropUnary sin sin e x

-- prop_cos :: EpsV Double -> AF s Double -> Property
-- prop_cos (EpsV e) x = correctnessPropUnary cos cos e x

--
-- Testing boilerplate
--

return [] -- This is a hack to make the quickCheckAll template work correctly

main :: IO ()
main = do
  $quickCheckAll
  return ()

