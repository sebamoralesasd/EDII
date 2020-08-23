data Tree a = E | L a | N (Tree a) (Tree a) deriving Show

partir :: Tree  a -> Tree (Tree a, a, Tree a)
partir arbol = aux arbol E E 
               where aux E _ _ = E
                     aux (L dato) izqAcum derAcum = L (izqAcum, dato, derAcum)
                     aux (N izq der) izqAcum derAcum = let (l , r) = aux izq izqAcum (N der derAcum) ||| aux der (N izqAcum izq) derAcum 
                                                       in N l r
