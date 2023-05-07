{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ChurchBoolean (ChurchBoolean (..), true, false, choice) where

newtype ChurchBoolean = CB (forall a. a -> a -> a)

instance Show ChurchBoolean where
    show :: ChurchBoolean -> String
    show (CB b) = b "True" "False"

instance Eq ChurchBoolean where
    (==) :: ChurchBoolean -> ChurchBoolean -> Bool
    (==) (CB m) (CB n) = m True False == n True False

true :: ChurchBoolean
true = CB const

false :: ChurchBoolean
false = CB $ \_ y -> y

choice :: ChurchBoolean -> a -> a -> a
choice (CB b) = b