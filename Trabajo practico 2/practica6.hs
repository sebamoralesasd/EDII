module Seis where

import Seq
import ArrSeq
import Par

-- Ej 4
data Paren = Open | Close
type Tupla = (Int, Int)

-- b
parenValScan :: Paren -> Int
parenValScan Open = 1
parenValScan Close = -1

matchParenScan :: (Seq s) => s Paren -> Bool
matchParenScan s = let sVal = mapS parenValScan s
                       (ss, r) = scanS (+) 0 sVal
                       ssBool = mapS (\x-> x>=0) ss
                   in r==0 && (reduceS (&&) True ssBool)


-- 6
conSig :: (Seq s) => s Int -> s (Int, s Int)
conSig ar = tabulateS (\i ->
                        ((nthS ar i), (dropS ar (i+1)))) (lengthS ar)

getMultiplos :: (Seq s) => (Int, s Int) -> Int
getMultiplos (a, se) =
  let m = mapS (\i -> if (mod a i) == 0 then 1 else 0) se
  in reduceS (+) 0 m

multiplos :: (Seq s) => s Int -> Int
multiplos ar =
  let conS = conSig ar
      totM = mapS (getMultiplos) conS
  in reduceS (+) 0 totM
