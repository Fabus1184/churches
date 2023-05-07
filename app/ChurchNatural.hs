{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ChurchNatural (ChurchNatural (..), zero) where

newtype ChurchNatural = CN (forall a. (a -> a) -> (a -> a))

instance Show ChurchNatural where
    show :: ChurchNatural -> String
    -- show as succ (pred n) == show n
    show n' = show' n' <> " = " <> show (fromEnum n')
      where
        show' :: ChurchNatural -> String
        show' (CN n) = n (\x -> "succ(" <> x <> ")") "0"

instance Eq ChurchNatural where
    (==) :: ChurchNatural -> ChurchNatural -> Bool
    (==) (CN m) (CN n) = m (const False) True == n (const False) True

instance Ord ChurchNatural where
    compare :: ChurchNatural -> ChurchNatural -> Ordering
    compare (CN m) (CN n) = m (const LT) EQ `compare` n (const GT) EQ

instance Num ChurchNatural where
    (+) :: ChurchNatural -> ChurchNatural -> ChurchNatural
    (+) (CN m) (CN n) = CN $ \f x -> m f (n f x)

    -- (-) :: ChurchN -> ChurchN -> ChurchN
    -- (-) (CN m) (CN n) = CN $ \f x -> n pred (m f) x
    (*) :: ChurchNatural -> ChurchNatural -> ChurchNatural
    (*) (CN m) (CN n) = CN $ \f x -> m (n f) x
    abs :: ChurchNatural -> ChurchNatural
    abs x = x
    signum :: ChurchNatural -> ChurchNatural
    signum = id
    fromInteger :: Integer -> ChurchNatural
    fromInteger n
        | n < 0 = error "Church numerals are non-negative"
        | n == 0 = CN $ \f x -> x
        | otherwise = succ $ fromInteger (n - 1)

instance Enum ChurchNatural where
    succ :: ChurchNatural -> ChurchNatural
    succ (CN n) = CN $ \f x -> f (n f x)
    pred :: ChurchNatural -> ChurchNatural
    pred (CN n) = CN $ \f x -> n (\g h -> h (g f)) (const x) id
    toEnum :: Int -> ChurchNatural
    toEnum = fromInteger . toInteger
    fromEnum :: ChurchNatural -> Int
    fromEnum (CN n) = n (+ 1) 0

zero :: ChurchNatural
zero = CN $ \f x -> x
