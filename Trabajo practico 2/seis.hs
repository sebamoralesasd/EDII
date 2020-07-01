module Seis where

import Seq
import ArrSeq
import Par

-- Funciones útiles
isEmptyS :: (Seq s) => s a -> Bool
isEmptyS ar = lengthS ar == 0

isSingleton :: (Seq s) => s a -> Bool
isSingleton ar = lengthS ar == 1

firstS :: (Seq s) => s a -> a
firstS ar = nthS ar 0

maxSeq :: (Seq s) => s Int -> s Int
maxSeq ar = fst $ scanS (max) 0 ar

dyc :: (Seq s) => s a -> (b -> b -> b) -> b -> (a -> b) -> b
dyc ar combine val base = reduceS combine val (mapS base ar)

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

-- Ej 4
data Paren = Open | Close
type Tupla = (Int, Int)

parenVal :: Tupla
parenVal = (0,0)

parenBase :: Paren -> Tupla
parenBase Open = (0,1)
parenBase Close = (1,0)

parenCombine :: Tupla -> Tupla -> Tupla
parenCombine (cI,aI) (cD,aD) = (max 0 (cD-aI+cI), max 0 (aI-cD+aD))

matchP :: (Seq s) => s Paren -> Tupla
matchP s = dyc s parenCombine parenVal parenBase


-- Ej 3
reverseSeq :: (Seq s) => s a -> s a
reverseSeq ar = let n = lengthS ar
                in tabulateS (\i -> nthS ar (n-1-i)) n

maxAgua :: (Seq s) => s Int -> (s Int, s Int)
maxAgua hist = maxSeq hist ||| reverseSeq (maxSeq (reverseSeq hist))

difAgua :: (Seq s) => s Int -> s Int -> s Int -> s Int
difAgua hist maxL maxR = tabulateS (dif) (lengthS hist)
  where dif i = max 0 $ min (nthS maxL i) (nthS maxR i) - nthS hist i

aguaHist :: (Seq s) => s Int -> Int
aguaHist hist =
  let (maxL, maxR) = maxAgua hist
      agua = difAgua hist maxL maxR
  in reduceS (+) 0 agua
