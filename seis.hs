module PracticaSeis where

import Seq
import Par
import ArrSeq

data Paren = Open | Close deriving Show
-- ~ Ejercicio 4-a)

matchP :: (Seq s) => s Paren -> (Int,Int)
matchP s = case showtS s of
           EMPTY -> (0,0)
           ELT Open -> (0,1) 
           ELT Close -> (1,0)
           NODE l r -> let ((closeL,openL),(closeR,openR)) = matchP l ||| matchP r
                       in if (openL <= closeR) 
                          then (closeL + closeR - openL, openR)
                          else (closeL, openR + openL - closeR)

matchParen :: (Seq s) => s Paren -> Bool
matchParen s = matchP s == (0,0)


-- ~ Ejercicio 4-b)
intToBool :: Int -> Bool
intToBool x = x >= 0 

parenToInt :: Paren -> Int
parenToInt Open = 1
parenToInt Close = (-1)

matchParenScan :: (Seq s) => s Paren -> Bool
matchParenScan s = let (seq,red) = scanS (+) 0 (mapS parenToInt s)
                   in red == 0 && (reduceS (&&) True (mapS intToBool seq))

-- ~ Ejercicio 6)
--Aclaración: si no separo esto como función aparte, el compilador se queja por no saber intuir el tipo del resultado
funcAux :: (Seq s) => (Int, s Int) -> s Int
funcAux (elem, seq) = filterS (\i -> mod elem i == 0) seq

--Aclaración: si no separo esto como función aparte, el compilador se queja por no saber intuir el tipo del resultado
funcAux2 :: (Seq s) => s Int -> s (Int, s Int)
funcAux2 seq = tabulateS (\i -> (nthS seq i, dropS seq (i+1))) (lengthS seq - 1)

cantMultiplos :: (Seq s) => s Int -> Int
cantMultiplos seq = let seq1 = funcAux2 seq
                        seq2 = mapS funcAux seq1
                        seq3 = mapS lengthS seq2
                    in  reduceS (+) 0 seq3    
