import Seq
import Par

module ListSeq where

tabulateSAux :: (Int -> a) -> Int -> Int -> [a]
tabulateSAux f nat valor = if nat == valor
                           then []
                           else let (x,xs) = (f valor) ||| (tabulateSAux f nat (valor+1))
                                in x:xs


instance Seq [] where

   emptyS = []
        
   singletonS x = [x]
   
   lengthS [] = 0
   lengthS (x:xs) = 1 + lengthS xs
       
   -- Suponemos indice valido
   nthS (x:xs) 0 = x
   nthS (x:xs) n = nthS xs (n-1)
   
   tabulateS f nat = tabulateSAux f nat 0
   
   mapS f [] = []
   mapS f (x:xs) = let (rx,rxs) = (f x) ||| (mapS f xs)
                   in rx:rxs
   
   filterS f [] = []
   filterS f (x:xs) = let (rx,rxs) = (f x) ||| (filterS f xs)
                      in if rx then x:rxs else rxs  

   appendS = (++)

   takeS [] _ = []
   takeS xs 0 = []
   takeS (x:xs) n = n : (takeS xs (n-1))
           
   dropS [] _ = []     
   dropS xs 0 = xs     
   dropS (x:xs) n = dropS xs (n-1)     
   
   showtS [] = EMPTY    
   showtS [x] = ELT x    
   showtS xs = let half = div (lengthS xs) 2
                   (h1,h2) = (takeS xs half) ||| (dropS xs half)
               in NODE h1 h2    
   
   showlS [] = NIL
   showlS (x:xs) = CONS x xs
   
   joinS xs = [elem | listaelem <- xs, elem <- listaelem]
   
   reduceS    
   
   scanS      
   
   fromList   
