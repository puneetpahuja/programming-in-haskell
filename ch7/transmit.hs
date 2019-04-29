import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParityBit :: [Bit] -> [Bit]
addParityBit bits = bits ++ [parityBit]
  where
    parityBit = if even ones then 0 else 1
    ones = sum bits

encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits = if even . sum $ bits then bits else error "Parity error"

removeParityBit :: [Bit] -> [Bit]
removeParityBit = init

decode :: [Bit] -> String
decode = map (chr . bin2int . removeParityBit . checkParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id -- tail
