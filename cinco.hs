module Cinco where

-- inorder
data BTree a = Empty | Node Int (BTree a) a (BTree a)
  deriving Show
--                          Altura (Izq) value (Der)

sizeBT :: BTree a -> Int
sizeBT Empty = 0
sizeBT (Node h _ _ _) = h

nth :: BTree a -> Int -> a
-- nth Empty _ = error ""
nth (Node h izq val der) index
  | index == (sizeBT izq) + 1 = val
  | index > (sizeBT izq) + 1 = nth der (index - ((sizeBT izq) + 1))
  | otherwise = nth izq (index - (sizeBT der))

cons :: a -> BTree a -> BTree a
cons item Empty = Node 1 Empty item Empty
cons item (Node h izq val der) = Node (h+1) (cons item izq) val der

tabulateAux :: (Int -> a) -> Int -> Int -> BTree a
tabulateAux f i n
  | i < n = cons (f i) (tabulateAux f (i+1) n)
  | i == n = Node 1 Empty (f n) Empty

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Empty
tabulate f n = tabulateAux f 1 n

mapBT :: (a -> b) -> BTree a -> BTree b
mapBT f Empty = Empty
mapBT f (Node h izq val der) = Node h (mapBT f izq) (f val) (mapBT f der)

takeBT :: Int -> BTree a -> BTree a
takeBT _ Empty = Empty
takeBT n arbol@(Node h izq val der)
  | n == 0 = Empty
  | n >= (sizeBT izq) + 1 =
    let cotaIzq = (sizeBT izq) + 1
        derTaken = takeBT (n - cotaIzq) der
      in Node (cotaIzq + (sizeBT derTaken)) izq val derTaken
  | otherwise = takeBT n izq

dropBT :: Int -> BTree a -> BTree a
dropBT _ Empty = Empty
dropBT n arbol@(Node h izq val der)
  | n == 0 = arbol
  | n < (sizeBT izq) + 1 =
    let izqDrop = dropBT n izq
        newSize = sizeBT izqDrop + sizeBT der + 1
        in Node newSize izqDrop val der
  | otherwise = dropBT (h - (sizeBT izq + 1)) der


---------------------------

data Tree a = E | Leaf a | Join (Tree a) (Tree a)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f E = E
mapT f (Leaf x) = Leaf (f x)
mapT f (Join l r) = let (l', r') = ((mapT f l), (mapT f r))
                    in Join l' r'

reduceT :: (a -> a -> a) -> a -> Tree a -> a
reduceT f e E = e
reduceT f e (Leaf x) = x
reduceT f e (Join l r) = let (l', r') = ((reduceT f e l), (reduceT f e r))
                         in f l' r'

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr
  where
    mr E = e
    mr (Leaf a) = f a
    mr (Join l r) = let (l', r') = (mr l, mr r)
                    in g l' r'

{-- (a, b, c, d) donde
  a = maxima suma subsecuencia contigua
  b = maxima suma de un prefijo
  c = maxima suma de un sufijo
  d = suma de todos los elementos
--}
mcssAux :: (Num a, Ord a) => Tree a -> (a,a,a,a)
mcssAux t = mapreduce f g (0,0,0,0) t
  where
    f v = (max v 0, max v 0, max v 0, v)
    g (a1,b1,c1,d1) (a2,b2,c2,d2) = (maximum [a1,a2,d1+c2],
                                     max b1 (b2+d1), max (c1+d2) c2,
                                     d1 + d2)

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = (\(a,b,c,d) -> a) (mcssAux t)
