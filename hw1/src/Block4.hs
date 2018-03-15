{-# LANGUAGE InstanceSigs #-}

module Block4 where

import Block3 (BSTree (..))

data Pair a = Pair a a
    deriving (Show)

data NonEmpty a = a :| [a]
    deriving (Show)

fromList :: [a] -> NonEmpty a
fromList (x:xs) = x:|xs
fromList _      = undefined

toList :: NonEmpty a -> [a]
toList (x:|xs) = x:xs

instance Foldable Pair 
  where
    foldMap :: Monoid m => (a -> m) -> Pair a -> m
    foldMap f (Pair x y) = f x `mappend` f y
    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f ac (Pair x y) = f x (f y ac)

instance Foldable NonEmpty 
  where
    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x:|xs) = f x `mappend` foldMap f xs
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f ac (x:|xs) = f x $ foldr f ac xs

instance Foldable BSTree 
  where
    foldMap :: Monoid m => (a -> m) -> BSTree a -> m
    foldMap _ Nil = mempty
    foldMap f (Node lst left right) = foldMap f left `mappend`
                                      foldMap f lst `mappend`
                                      foldMap f right

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn split = foldr add (fromList [[]])
            where
              add chr (x:|xs)
                            | chr == split = fromList ([]:x:xs)
                            | otherwise    = fromList ((chr:x):xs)

joinWith :: (Eq a) => a -> [[a]] -> [a]
joinWith chr = foldr1 (\s ac -> s `mappend` (chr:ac))
