-- examples.hs

module Examples where

rmdups :: Eq a => [a] -> [a]
rmdups [] = [] 
rmdups (x:xs) = x : rmdups (filter (/= x) xs)
