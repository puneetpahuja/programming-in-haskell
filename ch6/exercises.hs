-- exercises.hs

module Exercises where

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

(^:) :: Int -> Int -> Int
m ^: 0 = 1
m ^: n = m * (m ^: (n-1))

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | otherwise = euclid smaller diff
                         where
                           smaller = min x y
                           larger = max x y
                           diff = larger - smaller

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && (and' xs)


concat' :: [[a]] -> [a]
concat' []     = []
concat' ([] : ys) = concat' ys
concat' ((x:xs) : ys) = x : concat' (xs : ys)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x
