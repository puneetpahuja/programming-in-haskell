-- exercises.hs

module Exercises where

-- map f (filter p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = (x:xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr select []
            where
              select x ys | p x = x : ys
                          | otherwise = ys
