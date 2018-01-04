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

(!!:) :: [a] -> Int -> a
(x:xs) !!: 0 = x
(x:xs) !!: n = xs !!: (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                     = ys
merge xs []                     = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (firstHalf, secondHalf)
           where
             half       = length xs `div` 2
             firstHalf  = take half xs
             secondHalf = drop half xs

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort firstHalf) (msort secondHalf)
            where
              halves     = halve xs
              firstHalf  = (fst halves)
              secondHalf = (snd halves)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _     = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs
