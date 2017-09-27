module Parcialuno where


-- data Maybe a = Nothing | Just a
-- fun Nothing = False
-- fun (Just x) = True

-- data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show, Eq)
-- insert :: a -> Tree a -> Tree a
-- insert x t = Node x Empty t


-- makeMask "|..||" = [True, False, False, True, True]
-- makeMask "||." = [True, True, False]
-- makeMask ".|." = [False, True, False]
-- makeMask "" = []


makeMask xs = map (\x -> (if (x /= '|') && (x /= '.') then error "error: caracteres incorrectos encontrados" else x == '|')) xs


-- splitMult2 18 = [(1,18), (2,9), (3,6)]
-- splitMult2 3 = [(1,3)]
-- splitMult2 22 = [(1,22), (2,11)]
-- splitMult2 0 = []
-- splitMult2 (-1) = []

-- splitMult2 n = map (\x -> if x/n <= n then (x,n) else break) [0..]


-- falta sacar reptsss
splitMult2 n = filter (\(a,b) -> a<b) (map (\x -> (x,n `div` x)) (filter (\f -> ((n `mod` f) == 0)) [1..n]))


-- addI_D :: (Either Int Double) -> (Either Int Double) -> (Either Int Double)
-- addI_D (Left 1) (Left 2) = (Left 3)
-- addI_D (Left 1) (Right 2.3) = (Right 3.3)
-- addI_D (Right 1.45) (Right 2.3) = (Right 3.75)
-- addI_D (Right 1.45) (Left 6) = (Right 7.45)

-- addI_D l r = l `sum` r















