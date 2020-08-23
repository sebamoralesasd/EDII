data Bit = Zero | One deriving (Eq,Show)
type Vertex = Int
type Graph = [[Bit]]

invertir :: Bit -> Bit
invertir One = Zero
invertir Zero = One


inverso :: Graph -> Graph
inverso lista = [inverso2 xs | xs <- lista]

inverso2 :: [Bit] -> [Bit]
inverso2 xs = [invertir x | x <- xs]

inverso' :: Graph -> Graph
inverso' lista = [[(\x -> if x == One then Zero else One) x | x <- xs] | xs <- lista]


ventas :: Seq Float -> (Seq Float, Float)
