-- ~ Ejercicio 1: Dar el tipo completo de las siguientes funciones

-- ~ a) test f x = f x == x + 2

-- ~ b) esMenor y z =  y < z

-- ~ c) eq a b = a == b

-- ~ d) showVal x = "Valor: " ++ show x

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

