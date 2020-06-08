module ArrSeq where

import Par
import Seq
import qualified Arr as A


-- Funciones útiles
isEmptyAS :: A.Arr a -> Bool
isEmptyAS ar = lengthAS ar == 0

isSingleton :: A.Arr a -> Bool
isSingleton ar = lengthAS ar == 1

firstAS :: A.Arr a -> a
firstAS ar = nthAS ar 0

-- Funciones para la instancia Arr
emptyAS :: A.Arr a
emptyAS = A.empty

singletonAS :: a -> A.Arr a
singletonAS x = fromListAS [x]

lengthAS :: A.Arr a -> Int
lengthAS = A.length

nthAS :: A.Arr a -> Int -> a
nthAS = (A.!)

tabulateAS :: (Int -> a) -> Int -> A.Arr a
tabulateAS = A.tabulate

mapAS :: (a -> b) -> A.Arr a -> A.Arr b
mapAS f ar = tabulateAS (\i -> f(nthAS ar i)) (lengthAS ar)

filterAS :: (a -> Bool) -> A.Arr a -> A.Arr a
filterAS f ar = joinAS (tabulateAS (\i -> if f(ar A.! i)
                                  then singletonAS (ar A.! i)
                                  else emptyAS) (lengthAS ar))

appendAS :: A.Arr a -> A.Arr a -> A.Arr a
appendAS s t = joinAS (fromListAS [s,t])

takeAS :: A.Arr a -> Int -> A.Arr a
takeAS ar n = A.subArray 0 n ar

dropAS :: A.Arr a -> Int -> A.Arr a
dropAS ar n = A.subArray n ((lengthAS ar) - n) ar

showtAS :: A.Arr a -> TreeView a (A.Arr a)
showtAS ar
  | isEmptyAS ar = EMPTY
  | isSingleton ar = ELT (firstAS ar)
  | otherwise = let nElem = div (lengthAS ar) 2
                    (l,r) = (takeAS ar nElem) ||| (dropAS ar nElem)
                in NODE l r

showlAS :: A.Arr a -> ListView a (A.Arr a)
showlAS ar
  | isEmptyAS ar = NIL
  | otherwise = let (h, tl) = (nthAS ar 0) ||| (dropAS ar 1)
                in CONS h tl

joinAS :: A.Arr (A.Arr a) -> A.Arr a
joinAS = A.flatten

contractAS :: (a -> a -> a) -> A.Arr a -> Int -> A.Arr a
contractAS f ar n = tabulateAS (\i ->
  if i == div n 2
    then nthAS ar ((lengthAS ar) - 1)
    else f (nthAS ar (i*2)) (nthAS ar (i*2 + 1))) (div (n+1) 2)

reduceAS :: (a -> a -> a) -> a -> A.Arr a -> a
reduceAS f e ar
  | isEmptyAS ar = e
  | isSingleton ar = f e (firstAS ar)
  | otherwise = let arCont = contractAS f ar (lengthAS ar)
       in reduceAS f e arCont

scanAS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanAS = undefined

fromListAS :: [a] -> A.Arr a
fromListAS = A.fromList
