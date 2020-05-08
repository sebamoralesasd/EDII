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


-- ~ Ejercicio 9) completo?
data AGTree a = Node a [AGTree a ]
ponerProfs n (Node x xs) = Node n (map (ponerProfs (n + 1)) xs)

Principio de inducción (estructural) para AGTree:
Dada una propiedad P sobre AGTree, para probar que para todo agt :: AGTree a . P(agt)
- Probamos que P (Node a []) vale.
- Si tenemos xs :: [AGTree a] y (x :: AGTree a) es un elemento de la lista xs, 
  si probamos que P(x) vale para todo x elemento de xs, entonces P(Node a xs) vale.


-- ~ Ejercicio 13) preguntar como encarar la demostracion (por induccion estructural???)
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

- El rango de un heap es la longitud de la espina derecha (el
camino hacia la derecha hasta un nodo vacıo.)

- Invariante Leftist: el rango de cualquier hijo izquierdo es
mayor o igual que el de su hermano de la derecha.

-- ~
l1 (l2) es E entonces merge l1 l2 = l2 (l1) que es un leftist heap

sea l1 = (N x a1 b1 ) y l2 = (N y a2 b2 )
merge l1 l2 primero pregunta por el menor de los valores entre x e y
sabemos que, como l1 y l2 son leftist heaps, estos valores son los minimos valores de dichos heaps
en base a cual sea el menor, se llama a la funcion makeH con los argumentos correspondientes

makeH x a b se encarga de respetar la invariante del rango.
pregunta cual de los rangos de a y b son mayores y pone crea un nodo donde dicho heap va a la izquierda
el otro heap a la derecha, escribe su rango y su valor, y devuelve un heap cuya invariante leftist se respeta

Como se llama a makeH siempre con el minimo valor, la condicion de heap se respeta. Además vimos que makeH
respeta la invariante de leftist heap.

Por lo tanto merge l1 l2 es un leftist heap.



