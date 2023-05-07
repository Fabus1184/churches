{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ChurchFixed (ChurchFixed (..)) where

import Data.Proxy (Proxy (Proxy))
import Data.Ratio (denominator, numerator, (%))
import Debug.Trace (trace, traceShowId)
import GHC.TypeLits (KnownNat, Nat, natVal, symbolVal)

{-
\| Church encoded Fixed point numbers with type-level precision n
\| succ n = n + (1 / n)
-}
newtype ChurchFixed (n :: Nat) = CF (forall a. (a -> a) -> a -> a)

getPrecision :: forall n. KnownNat n => ChurchFixed n -> (forall a. Integral a => a)
getPrecision _ = fromIntegral $ natVal (Proxy :: Proxy n)

instance KnownNat n => Show (ChurchFixed n) where
    show :: ChurchFixed n -> String
    show (CF m) = show n <> " / " <> show (10 ^ getPrecision @n zero)
      where
        n = m (+ 1) 0

instance Eq (ChurchFixed n) where
    (==) :: ChurchFixed n -> ChurchFixed n -> Bool
    (==) (CF m) (CF n) = m (const False) True == n (const False) True

instance Ord (ChurchFixed n) where
    compare :: ChurchFixed n -> ChurchFixed n -> Ordering
    compare (CF m) (CF n) = m (const LT) EQ `compare` n (const GT) EQ

instance KnownNat n => Num (ChurchFixed n) where
    (+) :: ChurchFixed n -> ChurchFixed n -> ChurchFixed n
    (+) (CF m) (CF n) = CF $ \f x -> m f (n f x)

    -- (-) :: ChurchFixed n -> ChurchFixed n -> ChurchFixed n
    -- (-) (CF m) (CF n) = CF $ \f x -> n pred (m f) x
    (*) :: ChurchFixed n -> ChurchFixed n -> ChurchFixed n
    (*) (CF m) (CF n) = CF $ \f x -> m (n f) x
    abs :: ChurchFixed n -> ChurchFixed n
    abs x = x
    signum :: ChurchFixed n -> ChurchFixed n
    signum _ = 1
    fromInteger :: Integer -> ChurchFixed n
    fromInteger n = fromRational $ n % 1

instance KnownNat n => Real (ChurchFixed n) where
    toRational :: ChurchFixed n -> Rational
    toRational (CF n) = n (+ 1) 0 % (10 ^ getPrecision @n zero)

instance KnownNat n => Fractional (ChurchFixed n) where
    fromRational :: Rational -> ChurchFixed n
    fromRational r
        | r < 0 = error "Church numerals are non-negative"
        | r == 0 = CF $ \f x -> x
        | otherwise = foldr ($) zero (replicate succs succ)
      where
        p = getPrecision @n zero
        succs = round $ r * 10 ^ p
    (/) :: ChurchFixed n -> ChurchFixed n -> ChurchFixed n
    (/) (CF m) (CF n) = CF $ \f x -> m (n f) x

instance KnownNat n => Enum (ChurchFixed n) where
    succ :: ChurchFixed n -> ChurchFixed n
    succ (CF n) = CF $ \f x -> f (n f x)
    pred :: ChurchFixed n -> ChurchFixed n
    pred (CF n) = CF $ \f x -> n (\g h -> h (g f)) (const x) id
    toEnum :: Int -> ChurchFixed n
    toEnum = fromInteger . toInteger
    fromEnum :: ChurchFixed n -> Int
    fromEnum (CF n) = n (+ 1) 0

zero :: (ChurchFixed n)
zero = CF $ \f x -> x
