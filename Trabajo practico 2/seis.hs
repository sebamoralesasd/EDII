module Seis where

import Seq
import ArrSeq

-- Ej 1
divF :: Int -> Int -> Float
divF a b = fromIntegral a / fromIntegral b

promedios :: (Seq s) => s Int -> s Float
promedios ar =
  let (sumAr, tot) = scanS (+) 0 ar
      nElem = lengthS sumAr
  in tabulateS (\i -> if (i+1) == nElem
                      then divF tot nElem
                      else divF (nthS sumAr (i+1)) (i+1)) nElem

compara :: Int -> Int -> Int
compara a b = if a < b then 1 else 0

supera :: (Seq s) => s Int -> Int -> Int -> s Int
supera arB maxE nElem =
  tabulateS (\i ->
    if (i+1) == nElem
      then compara (nthS arB (i+1)) maxE
      else compara (nthS arB (i+1)) (nthS arB (i+2))) nElem

mayores :: (Seq s) => s Int -> Int
mayores ar =
  let (arB, maxE) = scanS (max) 0 ar
      nElem = (lengthS arB) - 1
      unos = supera arB maxE nElem
  in reduceS (+) 0 unos


-- Ej 2
type Mat = (Int, Int, Int, Int)

fibMat :: Mat
fibMat = (1,1,        -- (a, b,
          1,0)        --  c, d)

multMat :: Mat -> Mat -> Mat
multMat (a,b,c,d) (p,q,r,s) =
  (a*p + b*r, a*q + b*s,
   c*p + d*r, c*q + d*s)

getFib :: Mat -> Int
getFib (fi,_,_,_) = fi

fibSeq :: (Seq s) => Int -> s Int
fibSeq n = let ar = tabulateS (\_ -> fibMat) n
               matSeq = fst $ scanS (multMat) fibMat ar
           in mapS (getFib) matSeq
