module ArrSeq where

import Par
import Seq
import qualified Arr as A

emptyAS :: A.Arr a
emptyAS = A.empty

singletonAS :: a -> A.Arr a
singletonAS x = A.fromList [x]

lengthAS :: A.Arr a -> Int
lengthAS = A.length

nthAS :: A.Arr a -> Int -> a
nthAS = (A.!)

tabulateAS :: (Int -> a) -> Int -> A.Arr a
tabulateAS = A.tabulate

mapAS :: (a -> b) -> A.Arr a -> A.Arr b
mapAS f ar = A.tabulate (\i -> f(ar A.! i)) (A.length ar)

filterAS :: (a -> Bool) -> A.Arr a -> A.Arr a
filterAS f ar = A.flatten (A.tabulate (\i -> if f(ar A.! i)
                                  then singletonAS (ar A.! i)
                                  else A.empty) (A.length ar))

appendAS :: A.Arr a -> A.Arr a -> A.Arr a
appendAS s t = A.flatten (A.fromList [s,t])

takeAS :: A.Arr a -> Int -> A.Arr a
takeAS ar n = A.subArray 0 n ar

dropAS :: A.Arr a -> Int -> A.Arr a
dropAS ar n = A.subArray n ((A.length ar) - n) ar

showtAS :: A.Arr a -> TreeView a (A.Arr a)
showtAS ar = case lengthAS ar of
                0 -> EMPTY
                1 -> ELT (nthAS ar 0)
                n -> let nElem = div n 2
                         (l,r) = (takeAS ar nElem) ||| (dropAS ar nElem)
                         in NODE l r

showlAS :: A.Arr a -> ListView a (A.Arr a)
showlAS ar = let nElem = lengthAS ar
             in if nElem == 0 then NIL
                              else let (h, tl) = (nthAS ar 0) ||| (dropAS ar 1)
                                   in CONS h tl

joinAS :: A.Arr (A.Arr a) -> A.Arr a
joinAS = A.flatten

contractAS :: (a -> a -> a) -> A.Arr a -> A.Arr a
contractAS f ar =
  let nElem = (lengthAS ar)
  in tabulateAS (\i ->
      if i == div nElem 2
        then nthAS ar ((lengthAS ar) - 1)
        else f (nthAS ar (i*2)) (nthAS ar (i*2 + 1))) (div (nElem+1) 2)

reduceAS :: (a -> a -> a) -> a -> A.Arr a -> a
reduceAS = undefined

scanAS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanAS = undefined

fromListAS :: [a] -> A.Arr a
fromListAS = A.fromList
