module Lab01 where

import Data.List
import Data.Char

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
notA b = case b of
          True -> False
          False -> True

-- b) in es palabra reservada, por lo que no podes usarla para definir una funcion
inA [x]         =  []
inA (x:xs)      =  x : inA xs
inA []          =  error "empty list"

-- c) Length no es una funcion, length si
lengthx []        =  0
lengthx (_:l)     =  1 + lengthx l

-- d) el operador (:) toma un elemento y una lista, en ese orden
list123 = (1 : (2 : (3 : [])))

-- e) Las funciones prefijas se definen entre paréntesis
(++!) [] ys = ys
(++!) (x:xs) ys = x : ((++!) xs ys)
-- [1,2] ++! [3,4]
-- [1,2,3,4]

-- f) habia problemas con los ()
addToTail x xs = map (+x) (tail xs)

-- g) idem f)
listmin xs = head (sort xs)

-- h) (*) dumb programmer
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5 -}

five :: Num a => t -> a
five _ = 5

{-
b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado
-}
apply :: (a -> b) -> a -> b
apply f x = f x

{-
c) ident, la función identidad
-}
ident :: a -> a
ident x = x

{-
d) first, que toma un par ordenado, y devuelve su primera componente
-}

first :: (a,b) -> a
first (x,_) = x

{-
e) derive, que aproxima la derivada de una función dada en un punto dado
-}
derive :: (Fractional a, Fractional b) => ( b -> a) -> b -> a
derive f x = (f x+0.1 - f x) / 0.1

{-
f) sign, la función signo
-}
sign :: (Num a, Num a1, Ord a1) => a1 -> a
sign x | x>0    = 1
       | x == 0 = 0
       | x<0    = -1

{-
g) vabs, la función valor absoluto (usando sign y sin usarla)
-}
vabs :: (Num a, Ord a) => a -> a
vabs x = x * sign x

vabsAlt :: (Num a, Ord a) => a -> a
vabsAlt x | x<0       = -x
          | otherwise = x

{-
h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero
-}
pot :: (Integral b, Num a) => b -> a -> a
pot x a = a ^ x

{-
i) xor, el operador de disyunción exclusiva
-}
xor :: Bool -> Bool -> Bool
xor False False = False
xor True True   = False
xor _ _         = True

{-
j) max3, que toma tres números enteros y devuelve el máximo entre llos
-}
-- ~ Integral es subclase de Real (subclase de Num) que pertenece a Ord
max3 :: Integral a => a -> a -> a -> a
max3 a b c = max a (max b c)

{-
k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

bisiesto :: Integral a => a -> Bool
bisiesto x | ((mod x 4) /= 0)                                 = False
           | and [(mod x 100) == 0, (mod (div x 100) 4) /= 0] = False
           | otherwise                                        = True

{-
4)

Defina un operador infijo *$ que implemente la multiplicación de un
vector por un escalar. Representaremos a los vectores mediante listas
de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n
debe ser igual a la lista ns con todos sus elementos multiplicados por
n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

-}

(*$) :: Num a => [a] -> a -> [a]
(*$) xs a = map (*a) xs

v = [1, 2, 3] *$ 2 *$ 4

-- 5) Definir las siguientes funciones usando listas por comprensión:
--
-- a) 'divisors', que dado un entero positivo 'x' devuelve la
-- lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], mod x i == 0]
  -- | x > 0 = [i | i <- [1..x], mod x i == 0]
  -- | otherwise = []

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
-- de la lista los elementos distintos a 'x'

matches :: Int -> [Int] -> [Int]
matches x es = [i | i <- es, i == x]


{-
c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'
-}

cuadrupla n = [(a,b,c,d) | a <-[0..n], b<-[0..n], c<-[0..n], d<-[0..n], a^2 + b^2 == c^2 + d^2]

{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}

unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]

{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

scalarProduct :: [Int] -> [Int] -> Int
-- scalarProduct as bs = let ps = zip as bs
--                           zs = map (\(a,b) -> a*b) ps
--                       in sum zs

--scalarProduct as bs = sum (map (\(a,b) -> a*b) (zip as bs))
productupla (x,y) = x * y
scalarProduct xs ys = sum (map productupla (zip xs ys))

{-
7) Definir mediante recursión explícita
las siguientes funciones y escribir su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números
-}

suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs

{-
b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario
-}

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

{-c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario -}

todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

{- d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales -}

codes :: [Char] -> [Int]
codes [] = []
codes (x:xs) = ord x : codes xs

{- e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado -}

restos :: Integral a => [a] -> a -> [a]
restos [] n = []
restos (x:xs) n = (mod x n) : (restos xs n)

{-f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados -}

cuadrados :: Num a => [a] -> [a]
cuadrados [] = []
cuadrados (x:xs) = (x^2) : (cuadrados xs)

{- g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes -}

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (xs:xss) = length xs : longitudes xss

{- h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda -}

orden :: (Num a, Ord a) => [(a,a)] -> [(a,a)]
orden [] = []
orden ((x,y):xs) = if x<3*y then (x,y) : orden xs
                            else orden xs


{- i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares -}

pares :: Integral a => [a] -> [a]
pares [] = []
pares (x:xs) = if (mod x 2 == 0) then x : pares xs
                                 else pares xs

{- j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)
ESTO REQUIERE QUE IMPORTES EL MODULO DE CHAR (import Data.Char) -}


letras :: [Char] -> [Char]
letras [] = []
letras (x:xs) = if (isAlpha x) then x : letras xs
                               else letras xs

{- k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

masDe :: [[a]] -> Int -> [[a]]
masDe [] n = []
masDe (xs:xss) n = if (length xs >= n) then xs : (masDe xss n)
                                       else masDe xss n
