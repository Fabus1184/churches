{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ChurchTuple (ChurchTuple (..), fromCT, toCT, firstCT, secondCT) where

newtype ChurchTuple a b = CT (forall c. (a -> b -> c) -> c)

instance (Show a, Show b) => Show (ChurchTuple a b) where
    show :: ChurchTuple a b -> String
    show (CT t) = show (t (,))

fromCT :: ChurchTuple a b -> (a, b)
fromCT (CT t) = t (,)

toCT :: (a, b) -> ChurchTuple a b
toCT (a, b) = CT $ \f -> f a b

firstCT :: ChurchTuple a b -> a
firstCT (CT t) = t const

secondCT :: ChurchTuple a b -> b
secondCT (CT t) = t (\_ x -> x)
