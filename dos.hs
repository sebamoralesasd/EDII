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

-- ~ b) (0<)

-- ~ c) ('a':)

-- ~ d) (++"\n")

-- ~ e) filter (==7)

-- ~ f) map (++[1])


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

-- ~ b) if if then then else else

-- ~ c) False == (5>=4)

-- ~ d) 1 < 2 < 3

-- ~ e) 1 + if ('a' < 'z') then -1 else 0

-- ~ f) if fst p then fst p else snd p where p = (True,2)

-- ~ g) if fst p then fst p else snd p where p = (True,False)


-- ~ Ejercicio 5: Reescribir cada una de las siguientes definiciones sin usar let, where o if 

-- ~ a) f x = let (y, z ) = (x , x ) in y

-- ~ b) greater (x , y) = if x > y then True else False

-- ~ c) f (x , y) = let z = x + y in g (z , y) where g (a, b) = a − b


-- ~ Ejercicio 6: Pasar de notación Haskell a notación de funciones anónimas (llamada notación lambda)

-- ~ a) smallest, definida por
smallest (x, y, z) | x <= y && x <= z = x
                   | y <= x && y <= z = y
                   | z <= x && z <= y = z

-- ~ b) 
second x = \x -> x
    
-- ~ c) andThen, definida por
andThen True y  = y
andThen False y = False

-- ~ d) 
twice f x = f (f x )

-- ~ e) 
flip f x y = f y x

-- ~ f) 
inc = (+1)
    
-- ~ Ejercicio 7: Pasar de notación lambda a notación Haskell

-- ~ a) 
iff = \x -> \y -> if x then not y else y

-- ~ b) 
alpha = \x -> x
    
    
-- ~ Ejercicio 8: Dados los tipos de f y g, y la definicion de h: 

-- ~ f :: c -> d
-- ~ g :: a -> b -> c

-- ~ h x y = f (g x y)

-- ~ Determinar el tipo de h e indicar cuáles de las siguientes definiciones de h 
-- ~ son equivalentes a la dada:

-- ~ h = f . g
-- ~ h x = f . (g x )
-- ~ h x y = (f . g) x y

-- ~ Dar el tipo de la funcion (.)


-- ~ Ejercicio 9: La función zip3 zipea 3 listas. Dar una definición recursiva de la función 
-- ~ y otra definición con el mismo tipo que utilice la función zip. ¿Qué ventajas y desventajas 
-- ~ tiene cada definición?


-- ~ Ejercicio 10: Indicar bajo qué suposiciones tienen sentido las siguientes ecuaciones. 
-- ~ Para aquellas que tengan sentido, indicar si son verdaderas y en caso de no serlo modificar 
-- ~ su lado derecho para que resulten verdaderas:

-- ~ a) [[]] ++ xs = xs

-- ~ b) [[]] ++ xs = [xs]

-- ~ c) [[]] ++ xs = [] : xs

-- ~ d) [[]] ++ xs = [[], xs]

-- ~ e) [[]] ++ [xs] = [[], xs]

-- ~ f) [[]] ++ [xs] = [xs]

-- ~ g) [] ++ xs = [] : xs

-- ~ h) [] ++ xs = xs

-- ~ i) [xs] ++ [] = [xs]

-- ~ j) [xs] ++ [xs] = [xs, xs]


-- ~ Ejercicio 11: Inferir, de ser posible, los tipos de las siguientes funciones:
-- ~ (puede suponer que sqrt :: Float -> Float)

-- ~ a) 
modulus = sqrt . sum . map (^2)

-- ~ b) 
vmod []       = []
vmod (v : vs) = modulus v : vmod vs
    

-- ~ Ejercicio 12: Dado el siguiente tipo para representar números binarios:

type NumBin = [Bool]

-- ~ donde el valor False representa el número 0 y True el 1. Definir las siguientes operaciones 
-- ~ tomando como convención una representación Little-Endian (i.e. el primer elemento de las 
-- ~ lista de dı́gitos es el dı́gito menos significativo del número representado).

-- ~ a) suma binaria

-- ~ b) producto binario

-- ~ c) cociente y resto de la división por dos


-- ~ Ejercicio 13: Definir las siguientes funciones usando listas por comprensión:

-- ~ a) divisors, que dado un entero positivo x devuelve la lista de los divisores de x 
-- ~ (y la lista vacı́a si el entero no es positivo).

-- ~ b) matches, que dados un entero x y una lista de enteros descarta de la lista los 
-- ~ elementos distintos a x .

-- ~ c) cuadruplas, que dado un natural n, devuelve las cuadruplas (a, b, c, d) 
-- ~ con 0 < a, b, c, d <= n que cumplen a ^ 2 + b ^ 2 = c ^ 2 + d ^ 2.

-- ~ d) unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
-- ~ Por ejemplo, unique [1, 4, 2, 1, 3] = [1, 4, 2, 3].

-- ~ Ejercicio 14: El producto escalar de dos listas de enteros de igual longitud es la suma 
-- ~ de los productos de los elementos sucesivos (misma posición) de ambas listas. Usando listas 
-- ~ por comprensión defina una función scalarproduct que devuelva el producto escalar de dos listas.

-- ~ Sugerencia: Usar las funciones zip y sum.
