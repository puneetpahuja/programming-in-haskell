-- examples.hs

type String' = [Char]

type Pos = (Int, Int)
type Trans = Pos -> Pos

-- type Tree = (Int, [Tree])    -- wrong, cant have recursive types

type Pair a = (a, a)

newtype Nat a = Nat a

data A = Int

b :: Int
b = 1
