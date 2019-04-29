-- exercises.hs

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) n = n
mult (Succ m) n = add n (mult m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x t@(Node l y r) = occursHelper (compare x y) x t

occursHelper :: Ord a => Ordering -> a -> Tree a -> Bool
occursHelper EQ _ _ = True
occursHelper LT x (Node l _ _) = occurs x l
occursHelper GT x (Node _ _ r) = occurs x r

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r) = case c of EQ -> True
                                   LT -> occurs' x l
                                   GT -> occurs' x r
                                   where c = compare x y

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show
balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = balanced l && balanced r && abs (numLeaves l - numLeaves r) < 2

numLeaves :: Tree' a -> Int
numLeaves (Leaf' _) = 1
numLeaves (Node' l r) = numLeaves l + numLeaves r

t' :: Tree' Int
t' = Node' (Node' (Leaf' 1) (Node' (Leaf' 6) (Leaf' 9))) (Leaf' 4)

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance firstHalf) (balance secondHalf)
             where
               firstHalf = take half xs
               secondHalf = drop half xs
               half = length xs `div` 2

data Expr = Val Int | Add Expr Expr deriving Show
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

e :: Expr
e = Add (Add (Val 2) (Val 3)) (Val 4)

size :: Expr -> Int
size = folde (const 1) (+)

data Maybe' a = Nothing' | Just' a
instance Eq a => Eq (Maybe' a) where
  Nothing' == Nothing' = True
  Just' x == Just' y = x == y
  _ == _ = False

-- instance Eq a => Eq [a] where
--   [] == [] = True
--   x:xs == y:ys = x == y && xs == ys
--   _ == _ = False

-- tautology checker
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equi Prop Prop

-- assoc type
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval' :: Subst -> Prop -> Bool
eval' _ (Const b) = b
eval' s (Var x)   = find x s
eval' s (Not p)   = not (eval' s p)
eval' s (And p q) = eval' s p && eval' s q
eval' s (Or p q)  = eval' s p || eval' s q
eval' s (Imply p q) = eval' s p <= eval' s q
eval' s (Equi p q) = eval' s p == eval' s q

-- todo ex8 in exercise8AbstractMachine.hs
