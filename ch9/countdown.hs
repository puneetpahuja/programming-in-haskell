-- countdown.hs

data Op = Add | Sub | Mul | Div

data Expr = Val Int | Apply Op Expr Expr

eval :: Expr -> Int
eval Val x = x
eval Apply o x y = apply o (eval x) (eval y)

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

instance Show Expr where
  show Val n = show n
  show Apply o x y = "(" ++ show x ++ show o ++ show y ++ ")"

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Expr

solutions :: [Int] -> Int -> [Expr]
