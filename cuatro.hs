module Practica4 where

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
poner (e,p) (poner (e',p') q) = poner (e',max(p,p')) q   si e=e'
poner (e,p) (poner (e',p') q) = poner (e',p') (poner (e,p) q) si e!=e'
poner (e, p) (poner (e', p') q) = (poner (e', p') q) si p=p'

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

poner (e,p) {(e1,p1), ..., (en,pn)} = {(e,p)} U {(e1,p1), ..., (en,pn)} si e!=ei, p!=pi para todo i en [1..n]
poner (ex,px) {(e1,p1), ..., (en,pn)} = {(ex, max(px,pi))} U {(e1,p1), ..., (e(i-1), p(i-1)), (e(i+1), p(i+1)), ..., (en,pn)} si ex==e1 para algun i en [1..n]
poner (e,p) {(e1,p1), ..., (en,pn)} = {(e1,p1), ..., (en,pn)} si p=pi para algun i en [1..n]

primero {(e1,p1), ..., (en,pn)} = (e,p) donde p>=pi para todo i en [1..n]

sacar Q = Q - {primero Q}

esVacia Q = if #Q=0 then True else False

union {} Q = Q
union {(e1,p1), (e2,p2), ..., (en,pn)} Q = poner (e1,p1) (union {(e2,p2), ..., (en,pn)} Q)


-- ~ Ejercicio 9)
data AGTree a = Node a [AGTree a ]
ponerProfs n (Node x xs) = Node n (map (ponerProfs (n + 1)) xs)

Principio de inducción (estructural) para AGTree:
Dada una propiedad P sobre AGTree, para probar que para todo agt :: AGTree a . P(agt)
-- ~ - Probamos que P (Node a []) vale. (no es parte de la induccion, contemplado abajo)
- Si tenemos xs :: [AGTree a] y (x :: AGTree a) es un elemento de la lista xs,
  si probamos que P(x) vale para todo x elemento de xs, entonces P(Node a xs) vale.


-- ~ Ejercicio 13)
type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a)

merge :: Ord a ⇒ Heap a → Heap a → Heap a
merge h1 E                              = h1
merge E h2                              = h2
merge h1 @(N x a1 b1 ) h2 @(N y a2 b2 ) =
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
-- ~ Datos:
- El rango de un heap es la longitud de la espina derecha (el
camino hacia la derecha hasta un nodo vacıo.)

- Invariante Leftist: el rango de cualquier hijo izquierdo es
mayor o igual que el de su hermano de la derecha.

- Condición Heap: el valor de un nodo es menor que el valor de sus hijos

-- ~ Demostración
Hagamos inducción estructural en el segundo argumento de merge l1 l2
*Si l2 es E, entonces:
merge l1 l2 = l1 			<merge.1>
que es un leftist heap por hipotesis

*Sea l1 = (N x a1 b1) y l2 = (N y a2 b2)
consideremos como H.I. que (merge b1 l2) y (merge l1 b2) son leftist heaps [0]

merge l1 l2 primero pregunta por el menor de los valores entre x e y.
Sabemos que, como l1 y l2 son leftist heaps, estos valores son los minimos valores de dichos heaps.
Entonces sabemos que min (x,y) es menor que todo elemento en los heaps a1, a2, b1 y b2   [1]

En base a cual sea el menor, se llama a la funcion makeH con los argumentos correspondientes:
	makeH x a1 (merge b1 l2) si x es menor que y	[2]
	makeH y a2 (merge l1 b2) si y es menor que x	[2]

(Lema) makeH x a b se encarga de respetar la invariante del rango:
Dados x un elemento, a y b dos heaps, makeH pregunta cual de los rangos de a y b son mayores.
Crea un nodo donde el heap de mayor rango va a la izquierda, el heap de menor rango va a la derecha,
 se escribe el rango correpondiente y el valor x.
Vale aclarar que tanto a como b son leftist heaps por lo que respetan tanto
 la condicion de ser leftist como la de ser heaps. (ver [0] y [2])
Además, en la llamada de la función merge (ver [2]), el valor x que se pasa a
 la funcion makeH es siempre el valor minimo. Esto se ve claro considerando [1]

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
