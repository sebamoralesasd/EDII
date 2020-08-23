data Bst a = E | N (Bst a) a (Bst a)

minimum' :: Bst a -> a
minimum' (N E d r) = d
minimum' (N l d r) = minimum' l

delete :: Ord a => a -> Bst a -> Bst a
delete _ E = E
delete v t@(N l d r) | v < d  = N (delete v l) d r
                     | v > d  = N l d (delete v r)
                     | v == d = case t of
                                     (N E d E) -> E
                                     (N E d r) -> r
                                     (N l d E) -> l
                                     (N l d r) -> let y = minimum' r in N l y (delete y r)

split :: Ord a => a -> Bst a -> (Bst a, Maybe a, Bst a)
split _ E = (E, Nothing, E)
split v (N l d r) | v > d  = let (l',v',r') = split v r in (N l d l', v', r')
                  | v < d  = let (l',v',r') = split v l in (l', v', N r' d r)
                  | v == d = (l, Just d, r)

----------------------
-- ~ Especificación algebráica
-- ~ join empty b = b
-- ~ join (insert x a) b = insert x (join a b)

-- ~ delete x empty = empty
-- ~ delete x (insert y a) = if (x==y) then a else insert y (delete x a)
