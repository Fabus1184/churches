{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module ChurchList where

import Data.String (IsString, fromString)

newtype ChurchL a = CL (forall b. (a -> b -> b) -> b -> b)

instance Show a => Show (ChurchL a) where
    show :: ChurchL a -> String
    show (CL l) = "(" <> show' (CL l) <> ")"
      where
        show' :: Show a => ChurchL a -> String
        show' (CL l') = l' (\x y -> show x <> " : " <> y) "[]"

instance Semigroup (ChurchL a) where
    (<>) :: ChurchL a -> ChurchL a -> ChurchL a
    (<>) (CL m) (CL n) = CL $ \f x -> m f (n f x)

instance Monoid (ChurchL a) where
    mempty :: ChurchL a
    mempty = CL $ \_ x -> x

instance Foldable ChurchL where
    foldMap :: Monoid m => (a -> m) -> ChurchL a -> m
    foldMap f (CL l) = l (\x y -> f x <> y) mempty

    foldr :: (a -> b -> b) -> b -> ChurchL a -> b
    foldr f x (CL l) = l f x

instance IsString (ChurchL Char) where
    fromString :: String -> ChurchL Char
    fromString = fromList

fromList :: [a] -> ChurchL a
fromList xs = CL $ \f x -> foldr f x xs

toList :: ChurchL a -> [a]
toList (CL l) = l (:) []