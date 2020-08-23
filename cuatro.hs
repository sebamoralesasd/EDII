module Practica4 where

-- ~ Ejercicio 1)
tad List (A:Set) where
    import Bool
    nil: List A
    cons: A -> List A -> List A
    null: List A -> Bool
    head: List A -> A
    tail: List A -> List A
    
-- ~ Especificación algebráica
null nil = True
null (cons x xs) = False
head (cons x xs) = x
tail (cons x xs) = xs

-- ~ Especificación usando sequencias
nil = <>
cons x <x1, ..., xn> = <x, x1, ..., xn>
null <x1,...,xn> = True si n=0
null <x1,...,xn> = False si n!=0
head <x1,...,xn> = x1
tail <x1,x2,...,xn> = <x2,...,xn>

inL: List A -> A -> Bool
inL nil x = False
inL (cons y xs) x = or (y==x) (inL xs x)

del: List A -> A -> List A
del nil x = nil
del (cons y xs) x = if (x == y) then 
                       (del xs x) else
                       (cons y (del xs x))

-- ~ Ejercicio 2)
tad Pila (A:Set) where
    import Bool
    empty:   Pila A
    push:    A -> Pila A -> Pila A
    isEmpty: Pila A -> Bool
    top:     Pila A -> A
    pop:     Pila A -> Pila A

-- ~ Especificación algebráica
isEmpty empty = True
isEmpty (push x xs) = False
top (push x xs) = x
pop (push x xs) = xs

-- ~ Especificación usando sequencias
empty = <>
push x <x1,...,xn> = <x,x1,...,xn>
isEmpty <x1,...,xn> = True si n=0
isEmpty <x1,...,xn> = False en otro caso
top <x1,...,xn> = x1
pop <x1,x2,...,xn> = <x2,...,xn>

-- ~ Ejercicio 3)
tad Conjunto (A:Set) where
   import Bool
   vacio       : Conjunto A
   insertar    : A -> Conjunto A -> Conjunto A
   borrar      : A -> Conjunto A -> Conjunto A
   esVacio     : Conjunto A -> Bool
   union       : Conjunto A -> Conjunto A -> Conjunto A
   interseccion: Conjunto A -> Conjunto A -> Conjunto A
   resta       : Conjunto A -> Conjunto A -> Conjunto A

-- ~ Especifiación algebráica
insertar x (insertar x c) = insertar x c
insertar x (insertar y c) = insertar y (insertar x c)
borrar x vacio = vacio
borrar x (insertar x c) = c
borrar x (insertar y c) = insertar y (borrar x c)
esVacio vacio = True
esVacio (insertar x c) = False
union vacio b = b
union (insertar x c) b = insertar x (union c b)
inConj x vacio = False
inConj x (insertar y c) = if x == y then True else inConj x c
interseccion vacio b = vacio
interseccion (insertar x c) b = if (inConj x b) then insertar x (interseccion c b) else (interseccion c b)
resta b vacio = b
resta b (insertar x c) = borrar x (resta b c)

-- ~ Agregar choose
-- ~ Esto generaría inconsistencia con la especificación, pues uno no mantiene un orden al insertar elementos en un conjunto, 
-- ~ por lo que la función choose no sabría cual es el elemento que debería arrojar como resultado. Por ejemplo:
x = choose (insertar x (insertar y c)) = choose (insertar y (insertar x c)) = y --Genera una falla en la especificación.

-- ~ Ejercicio 4)
tad PQ (A:Set, B:Ordered Set) where
    import Bool
    vacia: PQ (A,B)
    poner: (A,B) -> PQ(A,B) -> PQ(A,B)
    primero: PQ(A,B) -> (A,B)
    sacar: PQ(A,B) -> PQ(A,B)
    esVacia: PQ(A,B) -> Bool
    union: PQ(A,B) -> PQ(A,B) -> PQ(A,B)
    
-- ~ Especificación algebráica:
poner (e,p) (poner (e,p') q) = poner (e,max(p,p')) q   

poner (e,p) (poner (e',p') q) = poner (e',p') (poner (e,p) q) 
poner (e, p) (poner (e', p) q) = (poner (e', p) q)  --Hay que corregir esto pues no pueden ocurrir estas dos
--Hablar con Guido por Jitsi?

poner (p1,v1) (poner (p2,v2) q) = if p1<>p2 then conmutan, else idem entrada

maxPriority (e,p) vacia = (e,p)
maxPriority (e,p) poner((e',p') q) = if p>p' then maxPriority (e,p) q else maxPriority (e',p') q

primero (poner (e,p) q) = maxPriority (e,p) q

eliminar (e,p) (poner (e',p') q) = if e==e' then q else (poner (e',p') (eliminar (e,p) q))

sacar poner (e,p) q = eliminar primero(poner (e,p) q) (poner (e,p) q)

esVacia vacia = True
esVacia poner (e,p) q = False

union vacia q' = q'
union (poner (e,p) q) q' = union q (poner (e,p) q')

-- ~ Especificacion usando conjuntos:
vacia = {}

-- ~ poner (e,p) {(e1,p1), ..., (en,pn)} = {(e,p)} U {(e1,p1), ..., (en,pn)} si e!=ei, p!=pi para todo i en [1..n]
-- ~ poner (ex,px) {(e1,p1), ..., (en,pn)} = {(ex, max(px,pi))} U {(e1,p1), ..., (e(i-1), p(i-1)), (e(i+1), p(i+1)), ..., (en,pn)} si ex==e1 para algun i en [1..n]
-- ~ poner (e,p) {(e1,p1), ..., (en,pn)} = {(e1,p1), ..., (en,pn)} si p=pi para algun i en [1..n]

PQ A = {(p,v) | p in P, v in A} donde no hay dos elementos co la misma prioridad

poner (p1,v1) pq = {(p1,v1)} union pq

primero {(e1,p1), ..., (en,pn)} = (e,p) donde p>=pi para todo i en [1..n]

sacar Q = Q - {primero Q}

esVacia Q = if #Q=0 then True else False

-- Preguntar Guido que quiso decir con comentario acá y como lo resolvería él
union {} Q = Q
union {(e1,p1), (e2,p2), ..., (en,pn)} Q = poner (e1,p1) (union {(e2,p2), ..., (en,pn)} Q) 

-- ~ Ejercicio 5)
tad BalT (A:Ordered Set) where
    import Maybe
    empty : BalT A
    join  : BalT A -> Maybe A -> BalT A -> BalT A
    size  : BalT A -> N
    expose: BalT A -> Maybe (BalT A, A, BalT A)

-- ~ Especificación algebráica
size empty = 0
size (join L Nothing R) = size L + size R
size (join L (Just x) R) = 1 + size L + size R

case expose t where
     empty             -> Nothing 
     join l (Just x) r -> Just (l,x,r)

-- ~ Ejercicio 6)
Queremos probar que (uncurry zip) ° unzip = id
Para ello, haremos inducción en el largo de la lista

n=0 -> L = []
(uncurry zip) ° unzip L = uncurry zip (unzip L) = uncurry zip (unzip []) =
uncurry zip ([],[]) = [] = L

n>0 -> L = (x,y):ps

(uncurry zip) ° unzip L =  							<def L>
uncurry zip (unzip (x,y):ps) = 						<def unzip>
uncurry zip (x:xs, y:ys) where (xs,ys) = unzip ps = <def uncurry zip>
(x,y) : (uncurry zip (xs,ys)) = 					<def del where>
(x,y) : (uncurry zip ps) = 							<hipotesis inductiva>
(x,y):ps = L

-- ~ Ejercicio 7)
Queremos probar que sum xs =< length xs * maxl xs, donde xs::[Nat] y maxl y sum se definen:

maxl []     = 0
maxl (x:xs) = x 'max' maxl xs

sum []     = 0
sum (x:xs) = x + sum xs

Para ello, haremos inducción en el largo de la lista.
n=0 -> xs = []

sum xs = 0 =< 0 * 0 = length xs * maxl xs

n>0 -> xs = (x:ys)
sum xs = sum (x:ys) = x + sum ys =< x + (length ys * maxl ys)

length xs * maxl xs = length (x:ys) * maxl (x:ys) = (1 + length ys) * (max x (maxl ys))   [1]

consideremos x =< maxl ys
[1]: (1 + length ys) * (maxl ys) = (maxl ys) + (length ys) * (maxl ys) >= x + (length ys) * (maxl ys) >=
x + sum ys = sum (x:ys) [Vale el enunciado]

consideremos x >= maxl ys
[1]: (1 + length ys) * x = x + length ys * x >= x + length ys * maxl ys >= x + sum ys = sum (x:ys) [Vale el enunciado]

Por lo tanto, vemos que la propiedad vale.

-- ~ Ejercicio 8)
data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

size :: Arbol a -> Int
size (Hoja a) = 1
size (Nodo a L R) = 1 + size L + size R

Haremos inducción estructural.
caso t = (Hoja n)
size t = 1 = 2*0 + 1

caso t = (Nodo a l r)
size t = 1 + size l + size r = 1 + 2k+1 + 2k'+1 = 2*(k+k'+1) + 1
-------------------------------------------------------------------
mirror :: Arbol a -> Arbol a
mirror Hoja a = Hoja a
mirror (Nodo a L R) = (Nodo a (mirror R) (Mirror L))

Haremos inducción estructural para probar que mirror°mirror = id

mirror°mirror Hoja a = mirror Hoja a = Hoja a

mirror°mirror (Nodo a L R) = mirror (Nodo a (mirror R) (mirror L)) = 
(Nodo a (mirror°mirror L) (mirror°mirror R)) = (Nodo a L R)
---------------------------------------------------------------------
Haremos inducción estructural para probar que hojas t < 2^(altura t)

caso t = Hoja a
hojas (Hoja a) = 1 < 2^(1) = 2^(altura (Hoja a))

caso t = (Nodo a l r)
hojas (nodo a l r) = hojas l + hojas r < 2^(altura l) + 2^(altura r) <  
2^(max (altura l) (altura r)) + 2^(max (altura l) (altura r)) = 2 * 2^(max (altura l) (altura r)) =
2^(1 + max (altura l) (altura r)) = 2^(altura t)

-- ~ Ejercicio 9)
data AGTree a = Node a [AGTree a ]
ponerProfs n (Node x xs) = Node n (map (ponerProfs (n + 1)) xs)

Principio de inducción (estructural) para AGTree:
Dada una propiedad P sobre AGTree, para probar que para todo agt :: AGTree a . P(agt)
-- ~ - Probamos que P (Node a []) vale. (no es parte de la induccion, contemplado abajo)
- Si tenemos xs :: [AGTree a] y (x :: AGTree a) es un elemento de la lista xs, 
  si probamos que P(x) vale para todo x elemento de xs, entonces P(Node a xs) vale.
 
-- ~ Ejercicio 10)
data Tree a = Leaf a | Node a (Tree a) (Tree a)

flatten (Leaf x)     = [x]
flatten (Node x l r) = flatten l ++ [x] ++ flatten r

mapTree f (leaf x)     = Leaf (f x)
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

Queremos probar que (map f) ° flatten == flatten ° (mapTree f)
Veamos por inducción estructural que esto vale
caso n = Leaf x
(map f) ° flatten n = (mapTree f) ° flatten (Leaf x) = (map f) [x] = [f x]
flatten ° (mapTree f) n = flatten ° (mapTree f) (Leaf x) = flatten (Leaf (f x)) = [f x]

caso n = Node x l r
(map f) ° flatten n = (map f) ° flatten (Node x l r) = (map f) (flatten l ++ [x] ++ flatten r) = [1]
(map f) ° flatten l ++ [f x] ++ (map f) ° flatten r =
flatten ° (mapTree f) l ++ [f x] ++ flatten ° (mapTree f) r =
flatten (Node (f x) (mapTree f l) (mapTree f r)) = flatten ° (mapTree f) n

[1] map f (a ++ b ++ c) = map f a ++ map f b ++ map f c

-- ~ Ejercicio 11)
-- join :: [[a]] -> [a]
join []       = []
join (xs:xss) = xs ++ join xss

Probar que join = concat ° map id
Veamos por induccion estructural en listas
caso t = []
concat ° map id t = concat ° map id [] = concat [] = [] = join []

caso t = (xs:xss)
join t = join (xs:xss) = xs ++ join xss = xs ++ concat (map id xss) = 
map id xs ++ concat (map id xss) = concat (map id (xs:xss))

Probaremos por induccion estructural en listas que: join ° join = join ° map join
caso t = []
join(join []) = join [] = join (map join [])

caso t = ((x:xs):xss) -- Faltan justificaciónes en el medio, pero leyendo se entienden
join(join t) = join(join ((x:xs):xss)) = join (((x:xs) ++ join xss)) =
join ( x:(xs ++ join xss)) = x ++ join(xs ++ join xss) =  <descomponemos xs>
x ++ y ++ ... ++ z ++ join ([] ++ join xss) = 
x ++ y ++ ... ++ z ++ join (join xss) = <HI>
x ++ y ++ ... ++ z ++ join (map join xss) = <recomponemos join para atras>
join (x:xs) ++ join (map join xss) = <def de join>
join ( join(x:xs) : (map join xss) ) = <reinsertamos el primer elemento en el map>
join (map join ((x:xs):xss))

-- ~ Ejercicio 12) HACER
data Bin a = Hoja | Nodo (Bin a) a (Bin a)

inorder :: Bin a -> [a]
inorder Hoja            = []
inorder (Nodo l dato r) =  inorder l ++ [dato] ++ inorder r


insert ::  Ord a => a -> Bin a -> Bin a
insert dato Hoja 	     = Nodo Hoja dato Hoja
insert dato (Nodo l a r) | dato=<a   = Nodo (insert dato l) a r
                         | otherwise = Nodo l a (insert dato r)


--t es un BST si es un Bin tal que:
--  t es hoja
--  t es Nodo l a r , donde l y r son BST; a >= y para todo y en l; a < y para todo y en r

-- ~ a) Probar que si t es un BST, entonces insert x t es un BST
Veamos por inducción estructural que insert x t es un BST

caso t = Hoja
insert x t = insert x Hoja = Nodo Hoja dato Hoja
Esto es trivialmente un BST pues ambas ramas son hojas y solo hay un dato en el medio.

caso t = Nodo l a r 

HI:= insert x l, insert x r son BST

insert x t = insert x (Nodo l a r) [1]

Analicemos que ocurre al comparar a con x
x <= a) insert x (Nodo l a r) = Nodo (insertar x l) a r
por HI sabemos que (insertar x l) es un BST, r es un BST y la condicion de orden se respeta pues ya hicimos la comparacion x <= a

x > a) el caso es analogo

Por lo tanto, insert x t resulta en un BST.

-- ~ b) Probar que si t es un BST, entonces inorder t es una lista ordenada
veamos por induccion estructural que inorder t es un BST

caso t = Hoja
inorder t = inorder Hoja = [] trivialmente ordenado

caso t = Node l a r
HI:= inorder l, inorder r son listas ordenadas
inorder t = inorder Node l a r = (inorder l) ++ [a] ++ (inorder r)

como inorder l nos da una lista ordenada, y sabemos que todo elemento de l es menor que a, al concatenar [a] a la derecha la lista sigue siendo ordenada.
del mismo modo, como inorder r es una lista ordenada con elementos mayores que a, al concatenar a la derecha de [a] mantenemos una lista ordenada.

por lo tanto, inorder t resulta en una lista ordenada. 

-- ~ Ejercicio 13)
type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a)

merge :: Ord a ⇒ Heap a → Heap a → Heap a
merge h1 E                              = h1
merge E h2                              = h2
merge h1 @(N r1 x a1 b1 ) h2 @(N r2 y a2 b2 ) =
        if x <= y then makeH x a1 (merge b1 h2)
                  else makeH y a2 (merge h1 b2)
                  
rank :: Heap a → Rank
rank E            = 0
rank (N r _ _ _ ) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b 
              then N (rank b + 1) x a b
              else N (rank a + 1) x b a

-- ~ Dado l1 y l2 leftist heaps, probar que merge l1 l2 es un leftist heap

- El rango de un heap es la longitud de la espina derecha (el
camino hacia la derecha hasta un nodo vacıo.)

- Invariante Leftist: el rango de cualquier hijo izquierdo es
mayor o igual que el de su hermano de la derecha.

- Condición Heap: el valor de un nodo es menor que el valor de sus hijos

-- ~ Metodo charlado
Hagamos inducción estructural en el segundo argumento de merge l1 l2
*Si l2 es E, entonces:
merge l1 l2 = l1 			<merge.1> 
que es un leftist heap por hipotesis

*Sea l1 = (N r1 x a1 b1) y l2 = (N r2 y a2 b2)

merge l1 l2 primero pregunta por el menor de los valores entre x e y.
Sabemos que, como l1 y l2 son leftist heaps, estos valores son los minimos valores de dichos heaps.
Entonces sabemos que min (x,y) es menor que todo elemento en los heaps a1, a2, b1 y b2   [1]

En base a cual sea el menor, se llama a la funcion makeH con los argumentos correspondientes:
	makeH x a1 (merge b1 h2) si x es menor que y	[2]
	makeH y a2 (merge h1 b2) si y es menor que x	[2]

(Lema) makeH x a b se encarga de respetar la invariante del rango:
Dados x un elemento, a y b dos heaps, makeH pregunta cual de los rangos de a y b son mayores.
Crea un nodo donde el heap de mayor rango va a la izquierda, el heap de menor rango va a la derecha, 
 se escribe el rango correpondiente y el valor x. 

Ahora, habría que aclarar que tanto a como b son leftist heaps
 por lo que respetan tanto la condicion de ser leftist como la de ser heaps.
Además, en la llamada de la función merge [2], el valor x que se pasa a la funcion makeH es siempre el valor minimo.
 Esto se ve claro considerando [1] 

Como se llama a makeH siempre con el minimo valor, la condicion de heap se respeta al construir el nuevo nodo
pues sabemos que x e y son menores que los valores de sus hijos. 
Además vimos que makeH respeta la invariante de leftist heap.

Por lo tanto merge l1 l2 es un leftist heap.


-- ~ Método opcional. Forma más matemática
-- ~ Propiedad Q: es leftist 
-- ~ Q(E) = True
-- ~ Q(N rnk valor izq der) = rank izq >= rank der && Q(izq) && Q(der)

-- ~ Propiedad H: es heap
-- ~ H(E) = True
-- ~ H(N rnk valor izq der) = valor < Dato izq && valor < Dato der && H(izq) && H(der)

-- ~ Propiedad QH:
-- ~ QH(t) := Q(t) && H(t)

-- ~ Lema para makeH
-- ~ hip) valor < min a && valor < min b && Q(a),Q(b),H(a),H(b) 
-- ~ tesis) QH(N rnk valor a b)

-- ~ Ejercicio 14)
Dar principio de induccion estructural para
data F = Zero | One F | Two (Bool -> F)

Dada una propiedad P sobre los elementos de F, P(f) valdrá para todo f::F si

-Vale P(Zero)
-Si vale P(n), entonces vale P(One n), donde n :: F
-Si valen P(f True) y P(f False), entonces vale P(Two f), donde f :: Bool -> F
