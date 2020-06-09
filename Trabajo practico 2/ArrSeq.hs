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


-- Función de contracción para reduce y para scan en arreglos
-- de tamaño impar
contractAS :: (a -> a -> a) -> A.Arr a -> Int -> A.Arr a
contractAS f ar n = tabulateAS (\i ->
  if i == div n 2
    then nthAS ar (i*2)
    else f (nthAS ar (i*2)) (nthAS ar (i*2 + 1))) (div (n+1) 2)

reduceAS :: (a -> a -> a) -> a -> A.Arr a -> a
reduceAS f e ar
  | isEmptyAS ar = e
  | isSingleton ar = f e (firstAS ar)
  | otherwise = let arCont = contractAS f ar (lengthAS ar)
       in reduceAS f e arCont

-- Función de contracción para scan en arreglos de tamaño par
contractParAS :: (a -> a -> a) -> A.Arr a -> Int -> A.Arr a
contractParAS f ar n = tabulateAS (\i -> let a = nthAS ar (2*i)
                                             b = nthAS ar (2*i + 1)
                                         in f a b) (div n 2)

-- Función de expansión
expandAS :: (a -> a -> a) -> A.Arr a -> A.Arr a -> Int -> A.Arr a
expandAS f arCont arOr n = tabulateAS (\i ->
  if mod i 2 == 0
    then nthAS arCont (div i 2)
    else let a = nthAS arCont (div i 2)
             b = nthAS arOr (i-1)
         in f a b) n

scanAS :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanAS f e ar
  | isEmptyAS ar = (ar, e)
  | isSingleton ar = let el = firstAS ar
                     in (singletonAS e, f e el)
  | otherwise = let nElem = lengthAS ar
                    arCont = if mod nElem 2 == 0
                               then contractParAS f ar nElem
                               else contractAS f ar nElem
                    (r, t) = scanAS f e arCont
                in (expandAS f r ar nElem, t)

fromListAS :: [a] -> A.Arr a
fromListAS = A.fromList


-- Instanciación de secuencia en arreglos
instance Seq A.Arr where
  emptyS = emptyAS
  singletonS = singletonAS
  lengthS = lengthAS
  nthS = nthAS
  tabulateS = tabulateAS
  mapS = mapAS
  filterS = filterAS
  appendS = appendAS
  takeS = takeAS
  dropS = dropAS
  showtS = showtAS
  showlS = showlAS
  joinS = joinAS
  reduceS = reduceAS
  scanS = scanAS
  fromList = fromListAS
