-- exercises.hs

module Exercises where

sumOfSquares :: Int -> Int
sumOfSquares n = sum [x^2 | x <- [1..n]]

sumOf100Squares :: Int
sumOf100Squares = sumOfSquares 100

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

grid1_2 :: [(Int, Int)]
grid1_2 = grid 1 2

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

square2 :: [(Int, Int)]
square2 = square 2

myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]

replicate3True :: [Bool]
replicate3True = myReplicate 3 True

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                       x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool
perfect n = sum (init (factors n)) == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]


ps = [(x, y) | x <- [1, 2], y <- [3, 4]]

find :: Eq a => a-> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]
