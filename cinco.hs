-- EJERCICIO 1
data BTree a = Empty | Node Int (BTree a) a (BTree a)

-- W->O(1)   S->O(1)
sizeBT :: Btree a -> Int
sizeBT Empty = 0
sizeBT (Node size _ _ _) = size


--W->O( lg n  )   S->O( lg n ) en caso de estar balanceado. Lineal en el largo de la secuencia en caso opuesto.
cons :: a -> BTree a -> BTree a
cons elem Empty = Nodo 1 Empty elem Empty
cons elem (Node size l d r) = (Node size+1 (cons elem l) d r)

--W->O( n + sum(W(f)) )   S->O( n + sum(W(f)) )
-- (Se podría paralelizar ambos let con || y podría quedar S(lg n + max Sf))
tabulateAux :: (Int -> a) -> Int -> Int -> Btree a
tabulateAux f 0 _ = Empty
tabulateAux f numero offset
   | (mod numero 2) == 0 = let mitadI = (div numero 2)
                               mitadD = (div numero 2) - 1
                               tabI = tabulateAux f mitadI offset
                               dato = f (mitadI + offset)
                               tabD = tabulateAux f mitadD (offset + mitadI + 1)
                           in (Node numero tabI dato tabD)
   | otherwise           = let mitad = (div numero 2)
                              tabI = tabulateAux f mitad offset
                              dato = f (mitadI + offset)
                              tabD = tabulateAux f mitad (offset + mitad + 1)
                           in (Node numero tabI dato tabD)

-- Depende de la funcion anterior
tabulate :: (Int -> a) -> Int -> BTree a
tabulate f numero = tabulateAux f numero 0

-- W->O(lg n)   S->O(lg n) en el mejor caso, si el arbol está balanceado.
-- En el peor caso, W y S son O(n) si el arbol tiene todos los hijos en la rama derecha, formando una
-- estructura de lista
takeBT :: Int -> BTree a -> BTree a
takeBT _ Empty = Empty
takeBT n (Node size l d r) | sizeL > n         = takeBT n l
                           | (sizeL + 1) == n  = (Node n l d Empty)
                           | otherwise         = (Node (min n size) l d (takeBT (n - sizeL - 1) r))
                           where sizeL = sizeBT l
--
-- =======
-- module Cinco where
--
-- -- inorder
-- data BTree a = Empty | Node Int (BTree a) a (BTree a)
--   deriving Show
-- --                          Altura (Izq) value (Der)
--
-- sizeBT :: BTree a -> Int
-- sizeBT Empty = 0
-- sizeBT (Node h _ _ _) = h
--
-- nth :: BTree a -> Int -> a
-- -- nth Empty _ = error ""
-- nth (Node h izq val der) index
--   | index == (sizeBT izq) + 1 = val
--   | index > (sizeBT izq) + 1 = nth der (index - ((sizeBT izq) + 1))
--   | otherwise = nth izq (index - (sizeBT der))
--
-- cons :: a -> BTree a -> BTree a
-- cons item Empty = Node 1 Empty item Empty
-- cons item (Node h izq val der) = Node (h+1) (cons item izq) val der
--
-- tabulateAux :: (Int -> a) -> Int -> Int -> BTree a
-- tabulateAux f i n
--   | i < n = cons (f i) (tabulateAux f (i+1) n)
--   | i == n = Node 1 Empty (f n) Empty
--
-- tabulate :: (Int -> a) -> Int -> BTree a
-- tabulate f 0 = Empty
-- tabulate f n = tabulateAux f 1 n
--
-- mapBT :: (a -> b) -> BTree a -> BTree b
-- mapBT f Empty = Empty
-- mapBT f (Node h izq val der) = Node h (mapBT f izq) (f val) (mapBT f der)
--
-- takeBT :: Int -> BTree a -> BTree a
-- takeBT _ Empty = Empty
-- takeBT n arbol@(Node h izq val der)
--   | n == 0 = Empty
--   | n >= (sizeBT izq) + 1 =
--     let cotaIzq = (sizeBT izq) + 1
--         derTaken = takeBT (n - cotaIzq) der
--       in Node (cotaIzq + (sizeBT derTaken)) izq val derTaken
--   | otherwise = takeBT n izq
--
-- dropBT :: Int -> BTree a -> BTree a
-- dropBT _ Empty = Empty
-- dropBT n arbol@(Node h izq val der)
--   | n == 0 = arbol
--   | n < (sizeBT izq) + 1 =
--     let izqDrop = dropBT n izq
--         newSize = sizeBT izqDrop + sizeBT der + 1
--         in Node newSize izqDrop val der
--   | otherwise = dropBT (h - (sizeBT izq + 1)) der
--
--
-- ---------------------------
--
-- data Tree a = E | Leaf a | Join (Tree a) (Tree a)
--
-- mapT :: (a -> b) -> Tree a -> Tree b
-- mapT f E = E
-- mapT f (Leaf x) = Leaf (f x)
-- mapT f (Join l r) = let (l', r') = ((mapT f l), (mapT f r))
--                     in Join l' r'
--
-- reduceT :: (a -> a -> a) -> a -> Tree a -> a
-- reduceT f e E = e
-- reduceT f e (Leaf x) = x
-- reduceT f e (Join l r) = let (l', r') = ((reduceT f e l), (reduceT f e r))
--                          in f l' r'
--
-- mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
-- mapreduce f g e = mr
--   where
--     mr E = e
--     mr (Leaf a) = f a
--     mr (Join l r) = let (l', r') = (mr l, mr r)
--                     in g l' r'
--
-- {-- (a, b, c, d) donde
--   a = maxima suma subsecuencia contigua
--   b = maxima suma de un prefijo
--   c = maxima suma de un sufijo
--   d = suma de todos los elementos
-- --}
-- mcssAux :: (Num a, Ord a) => Tree a -> (a,a,a,a)
-- mcssAux t = mapreduce f g (0,0,0,0) t
--   where
--     f v = (max v 0, max v 0, max v 0, v)
--     g (a1,b1,c1,d1) (a2,b2,c2,d2) = (maximum [a1,a2,d1+c2],
--                                      max b1 (b2+d1), max (c1+d2) c2,
--                                      d1 + d2)
--
-- mcss :: (Num a, Ord a) => Tree a -> a
-- mcss t = (\(a,b,c,d) -> a) (mcssAux t)
-- >>>>>>> chupi
