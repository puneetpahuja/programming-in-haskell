-- exercises.hs

import Data.Char
import System.IO

adder :: IO ()
adder = do 
  putStr "How many numbers? "
  n <- readInteger
  sum <- adder' 0 n
  putStrLn $ "The total is " ++ show sum
  

adder' :: Integer -> Integer -> IO Integer
adder' sum numsRemaining = if numsRemaining == 0 
  then return sum
  else do 
    x <- readInteger
    adder' (sum + x) (numsRemaining - 1)

readInteger :: IO Integer
readInteger = do
  n <- getLine
  if all isDigit n
    then return (read n :: Integer)
    else do
      putStrLn "Please enter a number."
      readInteger


adder2 :: IO ()
adder2 = do
  putStr "How many numbers? "
  n <- readInteger
  xs <- sequence [readInteger | _ <- [1 .. n]]
  putStr "The total is "
  print . sum $ xs 
  return ()

getCh :: IO Char
getCh = do 
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLine' :: String -> IO String
readLine' xs = do
  x <- getCh
  case x of
    '\n' -> do
      putChar x 
      return xs
    '\DEL' -> do
      putChar '\b'
      readLine' . safeInit $ xs 
    _ -> do 
      putChar x
      readLine' $ xs ++ [x]

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

readLine :: IO String
readLine = readLine' ""


