-- 1
-- [Char], (Char, Char, Char), [(Bool, Char)], ([Bool], [Char]), [[a] -> [a]]

-- 2
bools = [True, False]
nums = [[1, 2], [3, 4]]
add x y z = x + y + z
copy x = (x, x)
apply f x = f x


second xs = head (tail xs)
-- :t second
-- second :: [a] -> a

swap (x, y) = (y, x)
-- :t swap
-- swap :: (t1, t) -> (t, t1)

pair x y = (x, y)
-- :t pair
-- pair :: t -> t1 -> (t, t1)

double x = x * 2
-- :t double
-- double :: Num a => a -> a

palindrome xs = reverse xs == xs
-- :t palindrome
-- palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
-- :t twice
-- twice :: (t -> t) -> t -> t
