-- examples.hs

module Examples where

a = [x^2 | x <- [1..5]]

b = [(x, y) | y <- [4, 5], x <- [1..3]]

c = [(x, y) | x <- [1..3], y <- [x..3]]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]
