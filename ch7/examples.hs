-- examples.hs

module Examples where

rmdups :: Eq a => [a] -> [a]
rmdups [] = [] 
rmdups (x:xs) = x : filter (/= x) (rmdups xs)
