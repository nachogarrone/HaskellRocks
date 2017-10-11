module JuegoCartas where

import Data.List

data Naipe = Oros Int | Copas Int | Espadas Int | Bastos Int deriving (Eq, Show)
data FileSystem = Folder String [FileSystem] | File String Int deriving (Eq, Show)
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty deriving (Eq, Show)

suitNaipe (Oros _) = 1
suitNaipe (Copas _) = 2
suitNaipe (Espadas _) = 3
suitNaipe (Bastos _) = 4


numberNaipe (Oros n) = n
numberNaipe (Copas n) = n
numberNaipe (Espadas n) = n
numberNaipe (Bastos n) = n


baraja :: [Naipe]
baraja = [s n | s <- [Oros, Copas, Espadas, Bastos], n <- [1..12]]


turnoEscopa15 :: [Naipe] -> Naipe -> [Naipe]
turnoEscopa15 ns j
    | null combs = j:ns
    | otherwise = ns \\ concat combs
    where combs = [c | c <- powerest ns, sum (map numberNaipe (j:c)) == 15]


powerest [] = [[]]
powerest (x:xs) = p ++ (map (x:) p)
    where p = powerest xs


size (Folder _ []) = 0
size (File _ n) = n
size (Folder _ x) = sum (map size x)


preorder (Leaf n) = [n]
preorder (Empty) = []
preorder (Node n b1 b2) = [n] ++ (preorder b1) ++ (preorder b2)

inorder (Leaf n) = [n]
inorder (Empty) = []
inorder (Node n b1 b2) = (inorder b1) ++ [n] ++ (inorder b2)

postorder (Leaf n) = [n]
postorder (Empty) = []
postorder (Node n b1 b2) = (postorder b1) ++ (postorder b2) ++ [n]

binTreeSearch tree n = elem n (preorder tree)

isOrderedTree tree = ascending (inorder tree)

ascending [] = True
ascending [x] = True
ascending (x:y:xs) = x <= y && ascending (y:xs)


replaceBinTree Empty _ _ = Empty
replaceBinTree (Leaf n) v new = if (n == v) then new else Leaf n
replaceBinTree (Node n b1 b2) v new = if (n==v) then new else (if n<v then Node n (replaceBinTree b1 v new) b2 else Node n b1 (replaceBinTree b2 v new))


addOrderedTree Empty v = (Leaf v)
addOrderedTree (Leaf n) v
    | v <= n = (Node n (Leaf v) Empty)
    | otherwise = (Node n Empty (Leaf v))
addOrderedTree (Node n b1 b2) v
    | v <= n = Node n (addOrderedTree b1 v) b2
    | otherwise = Node n b1 (addOrderedTree b2 v)
