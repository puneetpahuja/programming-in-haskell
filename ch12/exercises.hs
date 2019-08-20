-- exercises.hs

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree