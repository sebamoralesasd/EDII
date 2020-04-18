module Practica3 where

import Data.List

-- ~ 1)
type Color = (Float,Float,Float)

mezclar :: Color -> Color -> Color
mezclar (r,g,b) (r',g',b') = ((r+r')/2 ,(g+g')/2,(b+b')/2)

-- ~ 2)
type Linea = (Int,[Char])

vacia :: Linea
vacia = (0,[])

moverIzq :: Linea -> Linea
moverIzq (0,linea) = (0,linea)
moverIzq (cursor,linea) = (cursor - 1,linea)

moverDer :: Linea -> Linea
moverDer (cursor, linea) = if (cursor == length linea) 
                           then (cursor,linea)
                           else (cursor + 1, linea)

moverIni :: Linea -> Linea
moverIni (_,linea) = (0,linea)

moverFin :: Linea -> Linea
moverFin (_,linea) = (length linea, linea)

insertar :: Char -> Linea -> Linea
insertar c (cursor,linea) = (cursor,(take cursor linea) ++ [c] ++ (drop cursor linea))

borrar :: Linea -> Linea
borrar (cursor, linea) = (cursor - 1, (take (cursor - 1) linea) ++ (drop cursor linea))


-- 3) Dado el tipo de datos
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

{-a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:

* Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.
* headCL y tailCL no están definidos para una lista vacı́a.
* headCL toma una CList y devuelve el primer elemento de la misma (el de más a la izquierda).
* tailCL toma una CList y devuelve la misma sin el primer elemento.
* isEmptyCL aplicado a una CList devuelve True si la CList es vacı́a (EmptyCL) o False en caso contrario.
* isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a) o False en caso contrario.-}

headCL :: CList a -> a
headCL (CUnit a) =  a
headCL (Consnoc l _ _) = l

snocCL :: CList a -> a -> CList a
snocCL EmptyCL dato          = CUnit dato
snocCL (CUnit a) dato        = Consnoc a EmptyCL dato
snocCL (Consnoc l xs r) dato = Consnoc l (snocCL xs r) dato

consCL :: a -> CList a -> CList a
consCL dato EmptyCL          = CUnit dato
consCL dato (CUnit a)        = Consnoc dato EmptyCL a
consCL dato (Consnoc l xs r) = Consnoc dato (consCL l xs) r 

tailCL :: CList a -> CList a
tailCL (CUnit a) = EmptyCL
tailCL (Consnoc _ xs r) = snocCL xs r

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit a) = True
isCUnit _ = False

-- b) Definir una función reverseCL que toma una CList y devuelve su inversa.
reverseCL :: CList a -> CList a
reverseCL xs = case xs of
                  EmptyCL          -> EmptyCL
                  (CUnit a)        -> (CUnit a)
                  (Consnoc l ys r) -> (Consnoc r (reverseCL ys) l)

-- c) Definir una función inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.
initaux :: CList a -> [CList a]
initaux EmptyCL = [EmptyCL]
initaux lista   = EmptyCL : (map (consCL (headCL lista)) (initaux (tailCL lista)))

listToCL :: [a] -> CList (a)
listToCL []     = EmptyCL
listToCL (x:xs) = consCL x (listToCL xs) 

initsCL :: CList a -> CList (CList a)
initsCL lista = listToCL (initaux lista)

-- d) Definir una función lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList.
lastsCL :: CList a -> CList (CList a)
lastsCL lista = listToCL (map (reverseCL) (initaux (reverseCL lista)))

-- e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas
concatdosCL :: CList a -> CList a -> CList a
concatdosCL l1 EmptyCL = l1
concatdosCL EmptyCL l2 = l2
concatdosCL (CUnit a) (CUnit b) = Consnoc a EmptyCL b
concatdosCL (CUnit a) (Consnoc l xs r) = Consnoc a (consCL l xs) r
concatdosCL (Consnoc l xs r) (CUnit a) = Consnoc l (snocCL xs r) a
concatdosCL (Consnoc l1 xs r1) (Consnoc l2 ys r2) = Consnoc l1 (concatdosCL (snocCL xs r1) (consCL l2 ys)) r2

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit xs) = xs
concatCL (Consnoc ls xss rs) = (concatdosCL (concatdosCL ls (concatCL xss)) rs)

-- ~ 4)Dado el siguiente tipo de dato:
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
--data Maybe a = Nothing | Just a

-- ~ a) Defina un evaluador eval :: Aexp → Int. ¿Cómo maneja los errores de división por 0?
eval :: Aexp -> Int
eval (Num a)    = a
eval (Prod a b) = (eval a) * (eval b)
eval (Div a b)  = div (eval a) (eval b)

-- ~ b) Defina un evaluador seval :: Aexp → Maybe Int.
seval :: Aexp -> Maybe Int
seval (Num a)    = Just a
seval (Prod a b) = case (seval a, seval b) of
                        (Nothing, Just v2)     -> Nothing
                        (Just v1, Nothing)     -> Nothing
                        ((Just v1), (Just v2)) -> Just (v1 * v2)
                        
seval (Div a b)  = case (seval a, seval b) of
                        (Nothing, Just v2)     -> Nothing
                        (Just v1, Nothing)     -> Nothing
                        ((Just v1), (Just v2)) -> if (v2 == 0) 
                                                  then Nothing 
                                                  else Just (div v1 v2)

-- ~ 5)Si un árbol binario es dado como un nodo con dos subárboles idénticos 
-- ~ se puede aplicar la técnica sharing para que los subárboles sean representados 
-- ~ por el mismo árbol. Definir las siguientes funciones de manera que se puedan
-- ~ compartir la mayor cantidad posible de elementos de los árboles creados:
data Tree a = HojaTree | NodoTree (Tree a) a (Tree a) deriving Show

-- ~ a) completo :: a → Int → Tree a, tal que dado un valor x de tipo a y un entero d,
-- ~ crea un árbol binario completo de altura d con el valor x en cada nodo.
completo :: a -> Int -> Tree a
completo valor 0           = HojaTree
completo valor profundidad = let subarbol = (completo valor (profundidad - 1))
                             in (NodoTree subarbol valor subarbol)

-- ~ b) balanceado ::a → Int → Tree a, tal que dado un valor x de tipo a y un entero n, 
-- ~ crea un árbol binario balanceado de tamaño n, con el valor x en cada nodo.
balanceado :: a -> Int -> Tree a
balanceado valor 0    = HojaTree
balanceado valor 1    = NodoTree HojaTree valor HojaTree
balanceado valor prof = if (even prof) then let subarbol = (balanceado valor (div prof 2))
                                            in (NodoTree subarbol valor subarbol)
                                       else let subI = (balanceado valor ((div prof 2)+1))
                                                subD = (balanceado valor (div prof 2))
                                            in (NodoTree subI valor subD)

-- 6) Dada las siguientes representaciones de árboles generales y de árboles binarios

data GenTree a = EmptyG | NodeG a [GenTree a]

data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a)

{-defina una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).-}

-- ~ g2btAux nodo hermanos
-- ~ nodo es el NodeG donde estoy parado y donde voy a armar el NodeB
-- ~ hermanos es la lista de hermanos de nodo, los cuales van a la derecha en NodeB
-- ~ si nodo tiene hijos, van a parar a la izquierda en NodeB

g2btAux :: GenTree a -> [GenTree a] -> BinTree a
g2btAux (NodeG a []) []         = NodeB EmptyB a EmptyB
g2btAux (NodeG a []) (x:xs)     = NodeB EmptyB a (g2btAux x xs)
g2btAux (NodeG a (x:xs)) []     = NodeB (g2btAux x xs) a EmptyB
g2btAux (NodeG a (x:xs)) (y:ys) = NodeB (g2btAux x xs) a (g2btAux y ys)

g2bt :: GenTree a -> BinTree a
g2bt EmptyG = EmptyB
g2bt (NodeG a lista) = g2btAux (NodeG a lista) []

-- 7) Definir las siguientes funciones sobre árboles binarios de búsqueda (bst):

data BST a = Hoja | Nodo (BST a) a (BST a)
--a) maximum, que calcula el máximo valor en un bst.

maximumBST :: BST a -> a
maximumBST (Nodo l value Hoja) = value
maximumBST (Nodo l v r)        = maximumBST r

--b) checkBST, que chequea si un árbol binario es un bst.

minimumBST :: BST a -> a
minimumBST (Nodo Hoja a r ) = a
minimumBST (Nodo l a r )    = minimumBST l


checkBST :: Ord a => BST a -> Bool
checkBST Hoja               = True
checkBST (Nodo Hoja v Hoja) = True
checkBST (Nodo l v Hoja)    = (v >= maximumBST l) && (checkBST l)
checkBST (Nodo Hoja v r)    = (v <= minimumBST r) && (checkBST r)
checkBST (Nodo l v r)       = (v <= minimumBST r) && (checkBST r) && (v >= maximumBST l) && (checkBST l)


-- ~ 8)

-- ~ 9)

-- ~ 10)
