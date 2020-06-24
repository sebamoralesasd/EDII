module Seis where

import Seq
import ArrSeq
import Numeric.Natural

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

type Nat = Natural

-- fibSeq :: (Seq s) => Nat -> s Nat
-- fibSeq = undefined
