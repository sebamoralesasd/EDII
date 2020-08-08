-- EJERCICIO 1
data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

-- W->O(1)   S->O(1)
sizeBT :: BTree a -> Int
sizeBT Empty = 0
sizeBT (Node size _ _ _) = size

nth :: BTree a -> Int -> a
nth (Node size l d r) n | n == sizeL = d
                        | n < sizeL  = nth l n
                        | otherwise  = nth r (n - sizeL - 1)
                        where sizeL = sizeBT l


--W->O( lg n  )   S->O( lg n ) en caso de estar balanceado. Lineal en el largo de la secuencia en caso opuesto.
cons :: a -> BTree a -> BTree a
cons elem Empty = Nodo 1 Empty elem Empty
cons elem (Node size l d r) = (Node size+1 (cons elem l) d r)


--W->O( n + sum(W(f)) )   S->O( n + sum(W(f)) )   
-- (Se podría paralelizar ambos let con || y podría quedar S(lg n + max Sf))
tabulateAux :: (Int -> a) -> Int -> Int -> BTree a
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

mapBT :: (a->b) -> BTree a -> BTree a
mapBT f Empty = Empty
mapBT f (Node size l d r) = Node size (mapBT f l) (f d) (mapBT f r)

-- W->O(lg n)   S->O(lg n) en el mejor caso, si el arbol está balanceado.
-- En el peor caso, W y S son O(n) si el arbol tiene todos los hijos en la rama derecha, formando una 
-- estructura de lista
takeBT :: Int -> BTree a -> BTree a
takeBT _ Empty = Empty
takeBT n (Node size l d r) | sizeL > n         = takeBT n l
                           | (sizeL + 1) == n  = (Node n l d Empty)
                           | otherwise         = (Node (min n size) l d (takeBT (n - sizeL - 1) r))
                           where sizeL = sizeBT l


dropBT :: Int -> BTree a -> BTree a
dropBT _ Empty = Empty
dropBT 0 t = t
dropBT n (Node size l d r) | sizeL >= n   = (Node (size-n) (dropBT n l) d r)
                           | otherwise   = dropBT (n - sizeL - 1) r
                           where sizeL = sizeBT l

-- EJERCICIO 2
data Tree a = E | Leaf a | Join (Tree a) (Tree a)

mapreduce :: (a->b) -> (b->b->b) -> b -> Tree a -> b
mapreduce f g e = mr where mr E          = e
                           mr (Leaf a)   = f a
                           mr (Join l r) = g (mr l) (mr r)

fMap :: Int -> (Int,Int,Int,Int)
fmap x = ((max x 0), (max x 0), (max x 0), x) 

gRed :: (Num a, Ord a) => (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
gRed (mSumL,mPrefL,mSufL,sumL) (mSumR,mPrefR,mSufR,sumR) = (max (max msumL msumR) (mSufL + mPrefR),
                                                            max mPrefL (sumL + mPrefR),
                                                            max mSufR (sumR + mSufL),
                                                            sumL + sumR)


mcss :: (Num a, Ord a) => Tree a -> a
mcss tree = let (a,b,c,d) = mapreduce fMap gRed (0,0,0,0) tree
            in a

-- ~ W mcss parece ser O(n), pues depende de mapreduce. Wf y Wg son O(1), pero mr es O(n).
-- ~ S es O(n), pero podría ser O(log n) si se paralelizara (mr l) y (mr r) usando |||

-- EJERCICIO 3
-- ~ data Tree a = E | Leaf a | Join (Tree a) (Tree a)

-- ~ mapreduce :: (a->b) -> (b->b->b) -> b -> Tree a -> b
-- ~ mapreduce f g e = mr where mr E          = e
                           -- ~ mr (Leaf a)   = f a
                           -- ~ mr (Join l r) = g (mr l) (mr r)

reduceT :: (a -> a -> a) -> a -> Tree a -> a
reduceT f e E = e
reduceT f e (Leaf a) = a
reduceT f e (Join a b) = f (reduceT f e a) (reduceT f e b)


smartJoin :: Tree Int -> Tree Int -> Tree Int
smartjoin t1 E = t1
smartjoin t1 t2 = join t1 t2

sufijos :: Tree Int -> Tree Int -> ( Tree (Int, Tree Int), Tree Int)
--         Arbol       acumulador    arbol tupla          acumulador resultante
sufijos E tAcum          = (Leaf (0,tAcum), tAcum)
sufijos h@(Leaf x) tAcum = (Leaf (x,tAcum), tRes) where tRes = (smartJoin h tAcum)
sufijos (Join l r) tAcum = (Join hijoIzq hijoDer, acumIzq)
                           where (hijoDer, acumDer) = sufijos r tAcum
                                 (hijoIzq, acumIzq) = sufijos l acumDer 

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos tree = t1 where (t1, _) = sufijos tree E

maxT :: Tree Int -> Int
maxT = reduceT max 0

maxValue :: (Int, Tree Int) -> Int
maxValue (compra, ventas) = (maxT ventas) - compra

maxAll :: Tree (Tree Int) -> Int
maxAll = mapreduce maxValue max 0

mejorGanancia :: Tree Int -> Int
mejorGanancia tree = maxAll (conSufijos tree)

-- EJERCICIO 4
data T a = Em | N (T a) a (T a)

-- ~ altura :: T a -> Int
-- ~ altura E = 0
-- ~ altura (N l x r) = 1 + max (altura l) (altura r)

seekEmBoy :: T a -> (T a, a)
seekEmBoy (N Em d Em) = (Em, d)
seekEmBoy (N Em d r) = (r, d)
seekEmBoy (N l d r) = ((N newl d r), raiz) where (newl, raiz) = seekEmBoy l

combinar :: T a -> T a -> T a
combinar Em t2 = t2
combinar t1 Em = t1
combinar (N Em d r) t2 = N t2 d r
combinar (N l d Em) t2 = N l d t2
combinar t1@(N l d r) t2 = N nuevot1 raizNueva t2 where (nuevot1, raizNueva) = seekEmBoy t1

combinarSimple :: T a -> T a -> T a
combinarSimple Em t2 = t2
combinarSimple t1 Em = t1
combinarSimple (N l d r) t2 = N t2 d (combinarSimple l r)

--------------
filterT :: (a -> Bool) -> T a -> T a
filterT f Em = Em
filterT f (N l d r) = if f d then N (filterT f l) d (filterT d r)
                             else combinarSimple (filterT f l) (filterT f d)

--------------
quickSortT :: T Int -> T Int
quickSortT Em = Em
quickSortT t@(N l d r) = let t = combinar l r
                             (l', r') = filter (<=d) t ||| filterT (>d) t
                             (l'',r'') = (quickSortT l') ||| (quickSortT r')
                         in N l'' d r''
-- WquicksortT(n) en el peor caso es O(n^2) + 2*Wfilter(n) pues el pivote es un caso extremo y me tira todo el arbol a un costado siempre
-- En el mejor caso, el pivote es el "elemento del medio" (visto como lista ordenada) y me distribuye 2 arboles de igual cantidad de nodos
-- por lo que el mejor caso queda O(n*Log(n)) + 2*Wfilter(n)


-- ~ Peor caso, los tenes todos como una lista a la izq(der) O(n^3)
-- ~ n = cantidad de elementos
-- ~ altura (t) = n-1
--W_q(n) = W_combinar(dl) + 2*W_filter(dt) + W_q(nl') + W_q(nr') + c
-- ~     = O(n-2) + 2*O(n-2^2) + W_q(n-1) + W_q(0) + c

-- ~ Mejor caso, arbol balanceado
--W_q(n) = W_combinar(log(n)) + 2*W_filter(log(n)) + W_q(nl') + W_q(nr') + c

-- ~ mejorcaso -> pivot en el medio (nlogn)
--W_q(n) = W_combinar(log(n)) + 2*W_filter(log(n)) + W_q(n/2) + W_q(n/2) + c
-- ~ peorcaso -> Pivot en la punta
--W_q(n) = W_combinar(log(n)) + 2*W_filter(log(n)) + W_q(n-1) + W_q(0) + c
-- EJERCICIO 5
splitAT :: BTree a -> Int -> (BTree a, BTree a)
splitAT Empty n = (Empty, Empty)
splitAT (Node size l d r) n | n <= sizeL       = let (mitadI, mitadD) = splitAT l n
                                                 in (mitadI, Node (size - n) mitadD d r)
                            | n == (sizeL + 1) = (Node sizeL l d Empty, r)
                            | otherwise        = let (mitadI, mitadD) = splitAT r (n-sizeL-1)
                                                 in (Node (size-n) l d mitadI, mitadD)  
                            where sizeL = sizeBT l
                            
-------------------------------
maxBT :: BTree a -> a
maxBT (Node _ l a Empty) = a
maxBT (Node _ l a r) = maxBT r

minBT :: BTree a -> a
minBT (Node _ Empty a r) = a
minBT (Node _ l a r) = minBT l

altura :: BTree a -> Int
altura Empty = 0
altura (Node _ l d r) = 1 + (max (altura l) (altura r))

insert :: Ord a => a -> BTree a -> BTree a
insert a Empty             = Node 1 Empty a Empty
insert a (Node size l d r) | a <= d    = Node (size+1) (insert a l) d r
                           | otherwise = Node (size+1) l d (insert a r)

delete :: Ord a => a -> BTree a -> BTree a
delete _ Empty                     = Empty
delete z (Node size l b r)         | z<b = Node (size-1) (delete z l) b r
delete z (Node size l b r)         | z>b = Node (size-1) l b (delete z r)
delete z (Node size Empty b Empty) | z==b = Empty
delete z (Node size Empty b r)     | z==b = r
delete z (Node size l b Empty)     | z==b = l
delete z (Node size l b r) 	       | z==b = let y = minBT r
                                            in Node (size-1) l y (delete y r)

-- se considera que el arbol es bst
rebalance :: Ord a => BTree a -> BTree a
rebalance Empty                    = Empty
rebalance n@(Node s Empty d Empty) = n
rebalance (Node s Empty d r)       = if ((altura r) <= 1) then Node s Empty d r
                                     else rebalance (Node s (insert d Empty) minD (rebalance (delete minD r)))
                                     where minD = minBT r
rebalance (Node s l d Empty)       = if ((altura l) <= 1) then Node s l d Empty
                                     else rebalance (Node s (rebalance (delete maxL l) ) maxL (insert d Empty))
                                     where maxL = maxBT l
rebalance (Node s l d r)           = if (abs (alturaL - alturaR) <= 1) then Node s (rebalance l) d (rebalance r)
                                     else if alturaL > alturaR then rebalance (Node s (rebalance(delete maxE l)) maxE (rebalance(insert d r)) )-- sacar max de l, ponerlo de raiz, insertar d en r, rebalancear
                                                               else rebalance (Node s (rebalance(insert d l)) minE (rebalance(delete minE r)) )-- sacar min de r, ponerlo de raiz, insertar d en l, rebalancear
                                     where alturaL = altura l
                                           alturaR = altura r
                                           maxE = maxBT l
                                           minE = minBT r


-- Forma usando split, mas facil...
rebalance' Empty = Empty
rebalance' t@(Node s l x r) = let (l', r') = splitAT t (div s 2)
                                  x' = nth 0 r
                                  (l'', r'') = rebalance' l' ||| rebalance' (drop 1 r')
                              in Node s l'' x' r''

h = altura = c. lg(n)
n = cantidad nodos
W(h) = O(h) + W (h-1) -> O(h 2^h) = O (h n)
