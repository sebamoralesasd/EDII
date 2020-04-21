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
                 deriving Show
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
search :: Ord k => [k] -> TTree k v -> Maybe v
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

