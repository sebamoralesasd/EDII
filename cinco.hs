module Cinco where

-- inorder
data BTree a = Empty | Node Int (BTree a) a (BTree a)
  deriving Show
--                          Altura (Izq) value (Der)

-- TODO: funcion para calcular altura

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
  | n >= (sizeBT izq) + 1 =
    let cotaIzq = (sizeBT izq) + 1
        derTaken = takeBT (n - cotaIzq) der
      in Node (cotaIzq + (sizeBT derTaken)) izq val derTaken
  | otherwise = takeBT n izq

dropBT :: Int -> BTree a -> BTree a
dropBT _ Empty = Empty
dropBT n arbol@(Node h izq val der)
  | n < (sizeBT izq) + 1 =
    let izqDrop = dropBT n izq
        newSize = sizeBT izqDrop + sizeBT der + 1
        in Node newSize izqDrop val der
  | otherwise = dropBT (h - (sizeBT izq + 1)) der
