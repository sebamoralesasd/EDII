module PracticaSeis where

import Seq
import Par
import ArrSeq

-- ~ Ejercicio 1a untested
promedios :: Seq Int -> Seq Float
promedios xs = let (seq,red) = scan (+) 0 xs
                    seq' = drop 1 (append seq (singleton red)) -- Pues el primer elemento de seq es 0, que no queremos
               in   tabulate (\i -> (nth i seq) / (i+1)) (length seq')

-- ~ Ejercicio 1b
mayores :: Seq Int -> Int -- 1 2 5 3 5 2 7 9
mayores seq = let (seq1,red) = scan max 0 seq --(<0 1 2 5 5 5 5 7 >,9)
                  seq2 = append (drop 2 seq1) (singleton red) -- <2 5 5 5 5 7 9>
              in length (group (==) seq2)


-- ~ Ejercicio 2
fibSeq : Nat -> Seq Nat
fibSeq n = map (\(a,b,c,d) -> b) 
               (first (scan (\(a1,b1,c1,d1) (a2,b2,c2,d2) -> (a1*a2 + b1*c2, a1*b2 + b1*d2, c1*a2 + d1*c2, c1*b2 + d1*d2)) 
                            (1,0,0,1) 
                            (tabulate (\x -> (1,1,1,0)) n)))

-- ~ Ejercicio 3
reverse :: Seq a -> Seq a
reverse seq = tabulate (\x -> nth ((length seq) - x - 1) seq) (length seq)

aguaHist :: Seq Nat -> Nat
aguaHist seq = let maxL = fst (scan max 0 seq)
                   maxR = reverse (fst (scan max 0 (reverse seq)))
                   agua = tabulate (\x -> max 0 ((min (nth x maxL) (nth x maxR)) - (nth x seq))) (length seq)
               in  reduce (+) 0 aguas

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

-- ~ Ejercicio 5a
sccml :: Seq Int -> Int
sccml seq = let val = (0,0,0,0)
                base v = (max v 0,max v 0,max v 0,v)
                combine (m,p,s,t) (m',p',s',t') = (max (max m m') (s+p'), max p (t + p'), max s' (t' + s), t + t')   
            in  reduce combine val (map base seq)
            
-- ~ Ejercicio 5b
sccml' :: (Seq s) => s Int -> Int
sccml' s = let x = scan (+) 0 s
               m = scan min (-100) x
           in max (tabulate (\j -> (nth j x) - (nth j m) (length s))


-- ~ Ejercicio 6)
--Aclaración: si no separo esto como función aparte, el compilador se queja por no saber intuir el tipo del resultado
modulos :: (Seq s) => (Int, s Int) -> s Int
modulos (elem, seq) = filterS (\i -> mod elem i == 0) seq

--Aclaración: si no separo esto como función aparte, el compilador se queja por no saber intuir el tipo del resultado
divisores :: (Seq s) => s Int -> s (Int, s Int)
divisores seq = tabulateS (\i -> (nthS seq i, dropS seq (i+1))) (lengthS seq - 1)

cantMultiplos :: (Seq s) => s Int -> Int
cantMultiplos seq = let seq1 = divisores seq
                        seq2 = mapS modulos seq1
                        seq3 = mapS lengthS seq2
                    in  reduceS (+) 0 seq3    

-- ~ Ejercicio 7
merge :: (a -> a -> Ordering) -> Seq a -> Seq a -> Seq a -- hecho de forma tosca. Como sería con dyc?
merge orden empty seq2 = seq2
merge orden seq1 empty = seq1
merge orden seq1 seq2  = if orden (nth seq1 0) (nth seq2 0) == LOWER
                         then append (take seq1 1) (merge orden (drop seq1 1) seq2)
                         else append (take seq2 1) (merge orden seq1 (drop seq2 1))

-- ~ reduce combine val (map base _)

sort :: (a -> a -> Ordering) -> Seq a -> Seq a
sort orden seq = reduce (merge orden) empty (map (\x-> singleton x) seq)

maxE :: (a -> a -> Ordering) -> Seq a -> a
maxE orden seq = reduce (\x y-> if orden x y == GREATER then x else y) (take seq 1) seq

maxS :: (a -> a -> Ordering) -> Seq a -> Nat
maxS orden seq = snd (reduce (\(x,xi) (y,yi)-> if orden x y == GREATER then (x,xi) else (y,yi) ) (nth 0 seq, 0) tabulate (\i -> (nth i seq, i)) (length seq))

group :: (a -> a -> Ordering) -> Seq a -> Seq a
group orden seq = reduce (\x y -> if (orden (nth (length x -1) x) (nth 0 y)) == Equal then append x (drop 1 y) else append x y) 
                         empty 
                         (map singleton seq)
                         

fAux :: (a,b) -> (a,Seq b)
fAux (k,v) = (k,singleton v)

collect :: Seq (a,b) -> Seq (a , Seq b)
collect seq = reduce (\x y -> if length x == 0 then y 
                              else if fst (nth (length x -1) x) == fst (nth 0 y)
                                   then append (take (length x -1) x) (append (( fst (nth 0 y), append (snd (nth (length x -1) x)) (snd (nth 0 y))  ) (drop 1 y)))
                                   else append x y) 
                     empty 
                     (map (\x -> singleton (fAux x)) (sort (\(a1,b2) (a2,b2) -> a1 <= a2) seq))

-- ~ Ejercicio 8
mapCollectReduce apv red s = let pairs = join (map apv s)
                                 groups = collect pairs
                             in map red groups

datosIngreso :: Seq (String, Seq Int) -> Seq (Int, Int)
datosIngreso s = mapCollectReduce apv red s

apv (nombre,notas) = let (suma, maxima) = reduce (\(a,b) (c,d)-> (a+c,max b d)) (0,0) (map (\x -> (x,x)) notas)
                         promedio = div suma (length notas)
                     in if promedio >= 70
                        then singleton ("Ingresa", maxima)
                        else if promedio >= 50
                             then singleton ("Lista de espera", maxima)
                             else singleton ("No Ingresa", maxima)

red (condicion,maximos) = (length maximos, reduce max 0 maximos)

-- ~ Ejercicio 9a
countCaract :: Seq (Seq Char) -> Seq (Char, Int)
countCaract colTextos = mapCollectReduce apv red colTextos

apv s = map (\c -> (c,1)) s
red (caracter,amount) = (caracter,reduce (+) 0 amount)

--marcas cada caracter como (caracter,aparece una vez)
-- juntas para que tener (caracter, secuencia de 1s)
-- reducis para tener (caracter,cantidad de veces que aparece)

-- ~ Ejercicio 9b
huffman :: Seq (Seq Char) -> Seq (Int, Seq Char)
huffman colTextos = collect (map (\(x,y) -> (y,x)) (countCaract colTextos))

-- conseguir secuencia de (caracter,cantidad de veces que aparece)
-- invertis -> (cantidad de veces que aparece, caracter)
-- juntas con collect -> Seq (cantidad de veces que aparece,seq de caracteres)
