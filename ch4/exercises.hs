-- exercises.hs

module Exercises where

halve :: [a] -> ([a], [a])
halve xs = (take halfLen xs, drop halfLen xs)
           where halfLen = length xs `div` 2

thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a
thirdB xs = xs !! 2

thirdC :: [a] -> a
thirdC (_:_:x:_) = x

safetailA :: [a] -> [a]
safetailA xs = if null xs then xs else tail xs

safetailB :: [a] -> [a]
safetailB xs | null xs = xs
             | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC (_: xs) = xs

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

-- False || False = False
-- _ || _ = True

-- False || b = b
-- True || _ = True

-- b || c | b == c    = b
--        | otherwise = True

(&&) a b = if a == True
              then if b == True
                      then True
                      else False
              else False

(&&) a b = if a == True
              then b
              else False

mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int
luhnDouble x | 2 * x > 9 = 2 * x - 9
             | otherwise = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn first second third fourth = mod (fourth + luhnDouble third + second + luhnDouble first) 10 == 0
