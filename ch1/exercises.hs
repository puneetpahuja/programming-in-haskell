-- exercises.hs

module Exercises where

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n:ns) = n * myProduct ns

reverseQsort :: Ord a => [a] -> [a]
reverseQsort [] = []
reverseQsort (x:xs) = reverseQsort larger ++ [x] ++ reverseQsort smaller
                      where
                        larger =  [a | a <- xs, a > x]
                        smaller = [b | b <- xs, b <= x]
