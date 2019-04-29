-- exercises.hs

module Exercises where

-- map f (filter p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr select []
            where
              select x ys | p x = x : ys
                          | otherwise = ys

dec2int' :: [Int] -> Int
dec2int' = foldl (\n x -> n*10 + x)  0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

decurry' :: (a -> b -> c) -> (a, b) -> c
decurry' f (x, y) = f x y

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f.head) tail 

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f 

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x | 2 * x > 9 = 2 * x - 9
             | otherwise = 2 * x

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap id luhnDouble . reverse
