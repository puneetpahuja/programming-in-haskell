-- numCoins.hs

numCoins :: Int -> [Int] -> Int
numCoins 0 _ = 0
numCoins _ [] = 99999999
numCoins total ys@(x:xs) | total < 0 = 99999999
                         | otherwise = min (numCoins (total - x) ys + 1) (numCoins total xs)
eX :: Double -> Double
eX x = sum [(fromIntegral ((round x) ^ n)) / (fromIntegral (factorial n)) | n <- [0..9]]

factorial :: Int -> Int
factorial z = product [1..z]


y :: Int
y = 5

z :: Int
z = 2


-- Enter your code here. Read input from STDIN. Print output to STDOUT

convert :: Int -> String
convert 1 = []
convert n = show n

helper :: String -> Char -> Int -> String -> String
helper xs x n [] = xs ++ [x] ++ (convert n)
helper xs x n (y:ys) | x == y = helper xs x (n + 1) ys
                     | otherwise = helper (xs ++ [x] ++ (convert n)) y 1 ys

compress :: String -> String
compress [] = []
compress (x:xs) = helper [] x 1 xs

-- main :: IO ()
--   do xs <- readLine
--      putStrLn . compress $ xs
