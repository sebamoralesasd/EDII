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

