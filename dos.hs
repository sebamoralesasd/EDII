module Practica2 where

import Data.List

-- ~ Ejercicio 1: Dar el tipo completo de las siguientes funciones

-- ~ a) test f x = f x == x + 2
test :: (Num a, Eq a) => (a -> a) -> a -> Bool 
test f x = f x == x + 2

-- ~ b) esMenor y z = y < z
esMenor :: Ord a => a -> a -> Bool
esMenor y z = y < z

-- ~ c) eq a b = a == b
eq :: Eq a => a -> a -> Bool
eq a b = a == b

-- ~ d) showVal x = "Valor: " ++ show x
showVal :: Show a => a -> [Char]
showVal x = "Valor: " ++ show x


-- ~ Ejercicio 2: Dar el tipo de las siguientes operaciones y explicar su propósito

-- ~ a) (+5)
a :: Num a => a -> a
a = (+5)

-- ~ b) (0<)
b :: (Num a, Ord a) => a -> Bool
b = (0<)

-- ~ c) ('a':)
c :: [Char] -> [Char]
c = ('a':)

-- ~ d) (++"\n")
d :: [Char] -> [Char]
d = (++"\n")

-- ~ e) filter (==7)
e :: (Eq a, Num a) => [a] -> [a]
e = filter (==7)

-- ~ f) map (++[1])
f :: (Num a) => [[a]] -> [[a]]
f = map (++[1])

-- ~ Ejercicio 3: Dar al menos dos ejemplos de funciones que tengan el tipo indicado en cada caso:

-- ~ a) (Int -> Int) -> Int

-- ~ b) Int -> (Int -> Int)

-- ~ c) (Int -> Int) -> (Int -> Int)

-- ~ d) Int -> Bool

-- ~ e) Bool -> (Bool -> Bool)

-- ~ f) (Int,Char) -> Bool

-- ~ g) (Int,Int) -> Int

-- ~ h) Int -> (Int,Int)

-- ~ i) a -> Bool

-- ~ j) a -> a


-- ~ Ejercicio 4: Indicar si cada una de las siguientes expresiones está o no bien formada. 
-- ~ En caso de que lo esté determinar el valor que denota, en caso contrario especificar si el error 
-- ~ es sintáctico o de tipos:

-- ~ a) if true then false else true where false = True ; true = False
-- ~ Valor que devuelve: False

-- ~ b) if if then then else else
-- ~ Error sintactico: if-then-else espera bool como 1° arg y dos sentencias que seguir como 2° y 3° argumento.

-- ~ c) False == (5>=4)
-- ~ Valor devuelto: False

-- ~ d) 1 < 2 < 3
-- ~ Error sintactico: pues es infix y no sabe si es izquierda o derecha
-- ~ si fuera con parentesis, seria error de tipo pues evaluar < devuelve un valor Bool. Volver a evaluar Bool < Int es un error

-- ~ e) 1 + if ('a' < 'z') then -1 else 0
-- ~ Valor devuelto: 0

-- ~ f) if fst p then fst p else snd p where p = (True,2)
-- ~ Error de tipo: se enoja con el valor 2, pues los valores de THEN y ELSE deben ser del mismo tipo

-- ~ g) if fst p then fst p else snd p where p = (True,False)
-- ~ Valor devuelto: True

-- ~ Ejercicio 5: Reescribir cada una de las siguientes definiciones sin usar let, where o if 

-- ~ a) f x = let (y, z) = (x , x) in y
f' = id

-- ~ b) greater (x , y) = if x > y then True else False
greater (x,y) = x > y

-- ~ c) f (x , y) = let z = x + y in g (z , y) where g (a, b) = a − b
f'' = fst

-- ~ Ejercicio 6: Pasar de notación Haskell a notación de funciones anónimas (llamada notación lambda)

-- ~ a) smallest, definida por
smallest (x, y, z) | x <= y && x <= z = x
                   | y <= x && y <= z = y
                   | z <= x && z <= y = z

-- ~ Primer intento, con varios argumentos
smallest' :: Ord a => a -> a -> a -> a
smallest' = (\x -> (\y -> (\z -> if (x <= y) then (if (x <= z) then x else z) else (if (y <= z) then y else z))))

-- ~ Segundo intento, con una tupla como argumento
smallest'' :: Ord a => (a,a,a) -> a
smallest'' = (\(x,y,z) -> if (x <= y) then (if (x <= z) then x else z) else (if (y <= z) then y else z))

-- ~ b) 
second x = \x -> x
second' = \x -> \y -> y
    
-- ~ c) andThen, definida por
andThen True y  = y
andThen False y = False

andThen' = \x -> \y -> if x then y else x

-- ~ d) 
twice f x = f (f x )

twice' f = \x -> f (f x)

twice'' = \f -> \x -> f (f x)

-- ~ e) 
flip2 f x y = f y x

flip2' = \f -> \x -> \y -> f y x

-- ~ f) 
inc = (+1)

inc' = \x -> x+1

    
-- ~ Ejercicio 7: Pasar de notación lambda a notación Haskell

-- ~ a) 
iff = \x -> \y -> if x then not y else y

iff' a b = if a then not b else b 

-- ~ b) 
alpha = \x -> x

alpha' x = x
    
    
-- ~ Ejercicio 8: Dados los tipos de f y g, y la definicion de h: 

-- ~ f :: c -> d
-- ~ g :: a -> b -> c

-- ~ h x y = f (g x y)

-- ~ Determinar el tipo de h e indicar cuáles de las siguientes definiciones de h 
-- ~ son equivalentes a la dada:

-- h :: a -> b -> d

-- ~ h = f . g => h x y = (f.g) x y =(por def de .)=> f (g x) y  ESTO ESTA MAL
-- ~ h x = f . (g x ) => h x y = f . (g x) y () =(por def de .)=> f ((g x) y) = f (g x y) = h x y CORRECTO
-- ~ h x y = (f . g) x y VER PRIMER ITEM

-- ~ Dar el tipo de la funcion (.)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- ~ Ejercicio 9: La función zip3 zipea 3 listas. Dar una definición recursiva de la función 
-- ~ y otra definición con el mismo tipo que utilice la función zip. ¿Qué ventajas y desventajas 
-- ~ tiene cada definición?
-- ~ zip3 :: [a] -> [b] -> [c] -> [(a,b,c)] ?? Nop, por definicion de abajo te queda [(a,(b,c)]
zip3'' x y z = zip x (zip y z)

-- ~ Esta definicion de zip3 es facil de codear, pero no resulta en la estructura de lista que uno esperaría
-- ~ Por otro lado, si las listas "y" y "z" son más largas que la lista "x", tardaran más tiempo que el necesario

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : (zip3' xs ys zs)

-- ~ Esta definicion de zip3 requiere que se estudien todos los casos para su desarrollo, pero resulta en 
-- ~ una estructura esperada. Cabe mencionar que trabaja en el largo de la lista mas corta

-- ~ Ejercicio 10: Indicar bajo qué suposiciones tienen sentido las siguientes ecuaciones. 
-- ~ Para aquellas que tengan sentido, indicar si son verdaderas y en caso de no serlo modificar 
-- ~ su lado derecho para que resulten verdaderas: (ver carpeta de clase, están resueltos ahí)

-- ~ a) [[]] ++ xs = xs
-- ~ [[]] ++ xs = []:xs con xs :: [[a]]

-- ~ b) [[]] ++ xs = [xs]
-- ~ Idem a

-- ~ c) [[]] ++ xs = [] : xs
-- ~ Correcto si xs :: [[a]]

-- ~ d) [[]] ++ xs = [[], xs]
-- ~ Idem a

-- ~ e) [[]] ++ [xs] = [[], xs]
-- ~ Correcto si xs :: [a]

-- ~ f) [[]] ++ [xs] = [xs]
-- ~ idem e

-- ~ g) [] ++ xs = [] : xs
-- ~ [] ++ xs = xs si xs :: [a]

-- ~ h) [] ++ xs = xs
-- ~ Correcto si xs :: [a]

-- ~ i) [xs] ++ [] = [xs]
-- ~ Correcto si xs :: a

-- ~ j) [xs] ++ [xs] = [xs, xs]
-- ~ Correcto si xs :: a


-- ~ Ejercicio 11: Inferir, de ser posible, los tipos de las siguientes funciones:
-- ~ (puede suponer que sqrt :: Float -> Float)

-- ~ a) 
-- ~ modulus :: [Double] -> Double segun GHCI, why not Float?
modulus = sqrt . sum . map (^2)

-- ~ b) vmod :: [[Double]] -> [Double]
vmod []       = []
vmod (v : vs) = modulus v : vmod vs
    

-- ~ Ejercicio 12: Dado el siguiente tipo para representar números binarios:

type NumBin = [Bool] 

-- ~ donde el valor False representa el número 0 y True el 1. Definir las siguientes operaciones 
-- ~ tomando como convención una representación Little-Endian (i.e. el primer elemento de las 
-- ~ lista de dı́gitos es el dı́gito menos significativo del número representado).

xor False True = True
xor True False = True
xor _ _        = False


acumular :: Bool -> Bool -> Bool -> Bool
acumular a b c = (a && b) || ((a || b) && c)

-- ~ a) suma binaria

sumaBinaria :: NumBin -> NumBin -> NumBin
sumaBinaria xs ys = sumBin xs ys False

sumBin :: NumBin -> NumBin -> Bool -> NumBin
sumBin [] [] True = [True]
sumBin [] [] False = []
sumBin (x:xs) [] acum = if (xor x acum) then (True:xs) else False : (sumBin xs [] acum)
sumBin [] (x:xs) acum = if (xor x acum) then (True:xs) else False : (sumBin [] xs acum)
sumBin (x:xs) (y:ys) acum = (xor acum (xor x y)) : (sumBin xs ys (acumular x y acum))

-- ~ b) producto binario
productoBinario :: NumBin -> NumBin -> NumBin
productoBinario _ [] = []
productoBinario [] _ = []
productoBinario [False] _ = [False]
productoBinario _ [False] = [False]
productoBinario xs (y:ys) = if y then sumaBinaria xs (productoBinario (False:xs) ys)
                                 else sumaBinaria [False] (productoBinario (False:xs) ys)

-- ~ c) cociente y resto de la división por dos
cociente :: NumBin -> NumBin
cociente = tail 

modBin :: NumBin -> NumBin
modBin (x:xs) = [x]
-- ~ Ejercicio 13: Definir las siguientes funciones usando listas por comprensión:

-- ~ a) divisors, que dado un entero positivo x devuelve la lista de los divisores de x 
-- ~ (y la lista vacı́a si el entero no es positivo).
divisors n = [ x | x <- [1..n], (mod n x) == 0 ]

-- ~ b) matches, que dados un entero x y una lista de enteros descarta de la lista los 
-- ~ elementos distintos a x .
matches x xs = [y | y <- xs, y == x]

-- ~ c) cuadruplas, que dado un natural n, devuelve las cuadruplas (a, b, c, d) 
-- ~ con 0 < a, b, c, d <= n que cumplen a ^ 2 + b ^ 2 = c ^ 2 + d ^ 2.
cuadrupla n = [(a,b,c,d) | a <-[0..n], b<-[0..n], c<-[0..n], d<-[0..n], a^2 + b^2 == c^2 + d^2]

-- ~ d) unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
-- ~ Por ejemplo, unique [1, 4, 2, 1, 3] = [1, 4, 2, 3].
unique xs = [x | (x,i) <- zip xs [0..], not (elem x (take i xs))]


-- ~ Ejercicio 14: El producto escalar de dos listas de enteros de igual longitud es la suma 
-- ~ de los productos de los elementos sucesivos (misma posición) de ambas listas. Usando listas 
-- ~ por comprensión defina una función scalarproduct que devuelva el producto escalar de dos listas.

-- ~ Sugerencia: Usar las funciones zip y sum.

productupla (x,y) = x * y
scalarProduct xs ys = sum (map productupla (zip xs ys))
