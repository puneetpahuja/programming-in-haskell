-- examples.hs

ff :: [] a -> [] a -- [a] is syntactic sugar for [] a
ff = id

-- how does IO functor satisfies second functor law (the side effect will be computed twice, but the result will be the same)