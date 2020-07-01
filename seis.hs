module PracticaSeis where

import Seq
-- ~ Ejercicio 4-b)

data Paren = Open | Close
data TreeView a = EMPTY | ELT a | NODE (Seq a) (Seq a)

intToBool :: Int -> Bool
intToBool x = if x >= 0 then True else False

matchParenScan :: Seq Paren -> Bool
matchParenScan s = let (seq,red) = scan (+) 0 (map (\i -> if i == Open then 1 else (-1)) s)
                   in red == 0 && (reduce (&&) True (map intToBool seq))


-- ~ Ejercicio 6)
