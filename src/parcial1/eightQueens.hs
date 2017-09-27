-- Torterolo Caamaño Fernandez
module EightQueens where
import Data.List


eightQueens_initial :: [Int]
eightQueens_initial = []

eightQueens_goalTest :: [Int] -> Bool
eightQueens_goalTest xs = (length xs == 8) && null (seComen xs)

eightQueens_successors :: [Int] -> [[Int]]
eightQueens_successors xs = [y:xs | y<-[0..7], notElem y xs]

-- donde estoy
-- seComen [2,4,7,3,0,6,1,5] deberia dar vacio
seComen :: [Int] -> [(Int,Int)]
seComen rs = [(n,m) | n <- [0..7] , m <- [n+1..7], abs (n-m) == abs((rs !! n) - (rs !! m))]

--eightQueens_initial
dfsearch :: (a->Bool) -> (a->[a]) -> a -> [a]
dfsearch gt sf i
 | gt i = [i]
 | otherwise =  concatMap (dfsearch gt sf) (sf i)
   -- [dfsearch gt sf x |  x <- sf i]
   -- map (\ix -> dfsearch gt sf ix ) (sf i)

-- map :: (a->b) - > [a] -> [b]
-- sf1 :: (a->[b]) - > [a] -> [[b]]
-- map :: (a->b) - > [a] -> [b]

test = dfsearch eightQueens_goalTest eightQueens_successors eightQueens_initial
