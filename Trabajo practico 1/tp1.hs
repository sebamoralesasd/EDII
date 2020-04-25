{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Tp1 where


{-
Integrantes: 
Ignacio Sebastián Moliné; Legajo M-6466/1
Sebastián Morales       ; Legajo M-
-}

-- ~ k := la clave en este nivel
-- ~ v := valor asociado a la clave. Maybe v en los nodos, solo v en las hojas

-- ~ La composicion de los nodos es la siguiente: si el siguiente valor 
-- ~ de la clave es menor al valor del nodo actual, buscamos en el nodo 
-- ~ izquierdo. Si es mayor, en el nodo derecho. Si es igual al nodo actual
-- ~ dependiendo de la operacion devolvemos el valor de maybe v o seguimos
-- ~ por el arbol del medio

data TTree k v = Node k (Maybe v ) (TTree k v ) (TTree k v ) (TTree k v )
                 | Leaf k v
                 | E
                 deriving (Show, Eq)
                 
t = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' Nothing E (Leaf 's' 1) E)
                                           (Node 'o' (Just 2) (Leaf 'd' 9)
                                                              E
                                                              (Leaf 's' 4))
                                           E)
                       (Node 's' Nothing E (Node 'i' (Just 4) (Leaf 'e' 8)
                                                              (Leaf 'n' 7)
                                                              E)
                                           E)



-- ~ Ejercicio 1) search :: Ord k ⇒ [k ] → TTree k v → Maybe v 
 -- ~ devuelve el valor asociado a una clave.
search :: Ord k => [k] -> TTree k v -> Maybe v -- testeado
search [] t                     = error "Clave vacia"
search _ E                      = Nothing
search (x:y:xs) (Leaf k v)      = Nothing
search [x] (Leaf k v)           = if (x == k) then (Just v) else Nothing
search [x] (Node k mv l m r)    = if (x == k) then mv else (if (x < k)
                                                            then search [x] l
                                                            else search [x] r)
search (x:xs) (Node k mv l m r) = if (x == k)
                                  then search xs m
                                  else (if (x < k)
                                        then search (x:xs) l
                                        else search (x:xs) r)

-- ~ Ejercicio 2) insert :: Ord k ⇒ [k ] → v → TTree k v → TTree k v 
-- ~ agrega un par (clave, valor) a un árbol. Si la clave ya está en el 
-- ~ árbol, actualiza su valor.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v -- Testeado
insert [] _ t                     = error "Clave vacia"
insert [x] v E                    = Leaf x v
insert [x] v (Leaf k v')          = if (x == k) 
                                    then Leaf k v 
                                    else (if (x < k)
                                          then Node k (Just v') (Leaf x v) E E
                                          else Node k (Just v') E E (Leaf x v)) 
insert [x] v (Node k mv l m r)    = if (x == k) 
                                    then (Node k (Just v) l m r) 
                                    else (if (x < k)
                                          then Node k mv (insert [x] v l) m r
                                          else Node k mv l m (insert  [x] v r)) 
insert (x:xs) v E                 = Node x Nothing E (insert xs v E) E
insert (x:xs) v (Leaf k v')       = if (x == k)
                                    then Node k (Just v') E (insert xs v E) E
                                    else (if (x < k)
                                          then Node k (Just v') (insert (x:xs) v E) E E
                                          else Node k (Just v') E E (insert (x:xs) v E))
insert (x:xs) v (Node k mv l m r) = if (x == k)
                                    then Node k mv l (insert xs v m) r
                                    else (if (x < k)
                                          then Node k mv (insert (x:xs) v l) m r
                                          else Node k mv l m (insert (x:xs) v r))

-- ~ Ejercicio 3) delete :: Ord k ⇒ [k] → TTree k v → TTree k v 
-- ~ elimina una clave y el valor asociada a ésta en un árbol.

-- ~ Ejercicio 4) keys :: TTree k v → [[k]]
-- ~ dado un árbol devuelve una lista ordenada con las claves del mismo.

-- ~ Ejercicio 5) Dar una instancia de la clase Dic para el tipo de datos TTree k v .

class Dic k v d | d -> k v where
    vacio    :: d
    insertar :: Ord k => k -> v -> d -> d
    buscar   :: Ord k => k -> d -> Maybe v
    -- ~ eliminar :: Ord k => k -> d -> d
    -- ~ claves   :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
    vacio = E
    insertar = insert
    buscar = search 
    -- ~ eliminar = delete
    -- ~ claves = keys
