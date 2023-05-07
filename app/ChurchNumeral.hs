{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ChurchNumeral (ChurchNumeral (..), zero) where

newtype ChurchNumeral = CN (forall a. (a -> a) -> (a -> a))

instance Show ChurchNumeral where
    show :: ChurchNumeral -> String
    -- show as succ (pred n) == show n
    show n' = show' n' <> " = " <> show (fromEnum n')
      where
        show' :: ChurchNumeral -> String
        show' (CN n) = n (\x -> "succ(" <> x <> ")") "0"

instance Eq ChurchNumeral where
    (==) :: ChurchNumeral -> ChurchNumeral -> Bool
    (==) (CN m) (CN n) = m (const False) True == n (const False) True

instance Ord ChurchNumeral where
    compare :: ChurchNumeral -> ChurchNumeral -> Ordering
    compare (CN m) (CN n) = m (const LT) EQ `compare` n (const GT) EQ

instance Num ChurchNumeral where
    (+) :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
    (+) (CN m) (CN n) = CN $ \f x -> m f (n f x)

    -- (-) :: ChurchN -> ChurchN -> ChurchN
    -- (-) (CN m) (CN n) = CN $ \f x -> n pred (m f) x
    (*) :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
    (*) (CN m) (CN n) = CN $ \f x -> m (n f) x
    abs :: ChurchNumeral -> ChurchNumeral
    abs x = x
    signum :: ChurchNumeral -> ChurchNumeral
    signum = id
    fromInteger :: Integer -> ChurchNumeral
    fromInteger n
        | n < 0 = error "Church numerals are non-negative"
        | n == 0 = CN $ \f x -> x
        | otherwise = succ $ fromInteger (n - 1)

instance Enum ChurchNumeral where
    succ :: ChurchNumeral -> ChurchNumeral
    succ (CN n) = CN $ \f x -> f (n f x)
    pred :: ChurchNumeral -> ChurchNumeral
    pred (CN n) = CN $ \f x -> n (\g h -> h (g f)) (const x) id
    toEnum :: Int -> ChurchNumeral
    toEnum = fromInteger . toInteger
    fromEnum :: ChurchNumeral -> Int
    fromEnum (CN n) = n (+ 1) 0

zero :: ChurchNumeral
zero = CN $ \f x -> x
