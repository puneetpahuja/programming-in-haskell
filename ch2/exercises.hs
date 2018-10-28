n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last2 ns = ns !! (length ns - 1)
last3 = head . reverse

init2 ns = take (length ns - 1) ns
init3 = reverse . tail . reverse

dummy :: [a] -> [b] -> Int
dummy xs ys = length xs + length ys
