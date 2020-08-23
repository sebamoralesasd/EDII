module PracticaSeis where

import Seq
import Par
import ArrSeq

infoCo :: (Seq s) => s (Int, Int) -> (Int, s Int)
infoCo seq = let seq' = mapS (\(x,y)-> x-y) seq                                              -- sequencia donde cada elemento registra el incremento o decremento de personas totales con covid
                 seq'' = tabulateS (\i -> ( fst (nthS seq i) ,i+1)) (lengthS seq) -- sequencia donde cada tupla representa (cantidad de casos nuevos, día que se registraron los casos)
                 (registro,red) = scanS (+) 0 seq'
                 registroFinal = appendS (dropS registro 1) (singletonS red)
                 mayorTupla = reduceS (\(x,y) (x',y') -> if x > x' then (x,y) else (x',y')) (0,0) seq''
             in (snd (mayorTupla), registroFinal)
