-- examples.hs

module Examples where

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

(*:) :: Int -> Int -> Int
m *: 0 = 0
m *: n = m + (m *: (n-1))

product' :: Num a => [a] -> a
product' []      = 1
product' (n:ns)  = n * product' ns

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

insert :: Ord a => a -> [a] -> [a]
insert x []                = [x]
insert x (y:ys) | x <= y   = x : y : ys
                |otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)


zip' :: [a] -> [b] -> [(a,b)]
zip' []     _      = []
zip' _      []     = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
