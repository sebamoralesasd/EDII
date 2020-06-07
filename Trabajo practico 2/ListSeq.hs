module ListSeq where

import Seq
import Par


tabulateSAux :: (Int -> a) -> Int -> Int -> [a]
tabulateSAux f nat valor = if nat == valor
                           then []
                           else let (x,xs) = (f valor) ||| (tabulateSAux f nat (valor+1))
                                in x:xs

contraerS :: (a -> a -> a) -> [a] -> [a]
contraerS f []       = []
contraerS f [x]      = [x]
contraerS f (x:y:xs) = let (xyC,xsC) = (f x y) ||| (contraerS f xs)
                       in xyC : xsC

expandirS :: (a -> a -> a) -> [a] -> [a] -> Int -> [a]
--funcion, sequencia original,sequencia contraida, indice, resultado
expandirS f [] [] indx                  = []
expandirS f [x] [x'] indx               = [x']
expandirS f s@(x:y:xs) s'@(x':xs') indx = if (mod indx 2) == 0
                                          then x' : (expandirS f s s' (indx+1))
                                          else let (head,tail) = (f x' x) ||| (expandirS f xs xs' (indx+1))
                                               in head : tail

instance Seq [] where

   emptyS = []
        
   singletonS x = [x]
   
   lengthS []     = 0
   lengthS (x:xs) = 1 + lengthS xs
       
   -- Suponemos indice valido
   nthS (x:xs) 0 = x
   nthS (x:xs) n = nthS xs (n-1)
   
   -- No anda si llamas tabulateS, pero si tabulateSAux... Por que?
   tabulateS f nat = tabulateSAux f nat 0
   
   mapS f []     = []
   mapS f (x:xs) = let (rx,rxs) = (f x) ||| (mapS f xs)
                   in rx:rxs
   
   filterS f []     = []
   filterS f (x:xs) = let (rx,rxs) = (f x) ||| (filterS f xs)
                      in if rx then x:rxs else rxs  

   appendS = (++)

   takeS [] _     = []
   takeS xs 0     = []
   takeS (x:xs) n = x : (takeS xs (n-1))
           
   dropS [] _     = []     
   dropS xs 0     = xs     
   dropS (x:xs) n = dropS xs (n-1)     
   
   --Hubo que modificar la declaración de TreeView para que tenga Show
   showtS []  = EMPTY    
   showtS [x] = ELT x    
   showtS xs  = let half = (div (lengthS xs) 2)
                    (h1,h2) = (takeS xs half) ||| (dropS xs half)
                in NODE h1 h2    
   
   --Hubo que modificar la declaración de ListView para que tenga Show
   showlS []     = NIL
   showlS (x:xs) = CONS x xs
   
   joinS xs = [elem | listaelem <- xs, elem <- listaelem]
   
   
   -- Buscar un caso de prueba en el que f no sea asociativa.
   -- reduceS    :: (a -> a -> a) -> a -> s a -> a
   reduceS f base []  = base
   reduceS f base [x] = f base x
   reduceS f base xs  = reduceS f base (contraerS f xs)
   
   -- Idem reduceS
   -- ~ scanS     :: (a -> a -> a) -> a -> s a -> (s a, a)
   scanS f base []  = (singletonS base, base)  
   scanS f base [x] = (singletonS base, f base x)
   scanS f base xs  = let xsC = contraerS f xs
                          (xsSeq,xsRed) = scanS f base xsC
                      in (expandirS f xs xsSeq 0, xsRed)
   
   -- ~ fromList   :: [a] -> s a
   -- como hago que esto imprima?
   fromList xs = xs
