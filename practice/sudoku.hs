-- sudoku.hs
-- 0 is for blank
-- block is the 3x3 board
-- each individual square is a box

type Pos = (Int, Int)

type Box = (Pos, Int)

type Board = [Box]

-- A move is valid if the position and value is within bounds. It does not check whether the value satisfies the uniqueness constraint.

-- a box is valid if it specifies the uniqueness constraints 
validBox :: Board -> Box -> Bool
validBox board box = validRow board box && validCol board box && validBlock board box

isFull :: Board -> Bool
isFull board = and [val /= 0 | (_, val) <- board]  

validVal :: Int -> Bool
validVal x = x >= 1 && x <= 9

validPos :: Pos -> Bool
validPos (x, y) = validCoord x && validCoord y

validCoord :: Int -> Bool
validCoord x = x >= 1 && x <= 9

validRow :: Board -> Box -> Bool
validRow board box@(_, val) = notElem val $ rowVals board box 

validCol :: Board -> Box -> Bool
validCol board box@(_, val) = notElem val $ colVals board box

validBlock :: Board -> Box -> Bool
validBlock board box@(_, val) = notElem val $ blockVals board box

-- gives the values in box's row minus the box's value 
rowVals :: Board -> Box -> [Int]
rowVals board ((row, col), val) = [v | ((x, y), v) <- board, x == row, y /= col]

-- similar to rowVals
colVals :: Board -> Box -> [Int]
colVals board ((row, col), val) = [v | ((x, y), v) <- board, y == col, x /= row]

-- similar to rowVals
blockVals :: Board -> Box -> [Int]
blockVals board ((row, col), val) = [v | ((x, y), v) <- board, x `elem` blockRows, y `elem` blockCols, x /= row || y /= col]
  where
    blockRows = blockCoords row
    blockCols = blockCoords col
    blockCoords z = let base = (z-1) `div` 3 in map (+ (base*3)) [1..3]

testBoard :: Board
testBoard = zip [(x, y) | x <- [1..9], y <- [1..9]] [0,0..]

validBoard :: Board -> Bool
validBoard board = all (validBox board) board

showBoard :: Board -> String
showBoard board = unlines $ bar : map showRow (chunks 9 board)
  where
    bar = ' ' : replicate 53 '_'

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

showRow :: [Box] -> String
showRow boxes = rowCeiling ++ "\n|" ++ concatMap showBox boxes ++ "\n" ++ rowBase
  where
    rowCeiling = "|" ++ (concat . replicate 9 $ "     |")
    rowBase = "|" ++ (concat . replicate 9 $ "_____|")

showBox :: Box -> String
showBox (_, val) = "  " ++ show val ++ "  |"

