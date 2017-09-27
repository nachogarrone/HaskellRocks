module AltoOrden where

import Data.Char
import Data.List


parseBits :: String -> [Int]
parseBits texto = map filtrarBits texto

filtrarBits :: Char -> Int
filtrarBits c = if elem c "01" then digitToInt(c) else error "error"

factorial :: Int -> Int
factorial a = if(a>0)then foldl (*) 1 [1..a] else error "error"

parseBin :: String -> Int
parseBin t = foldl (\x y -> x*2+y) 0 (parseBits t)

pairMap :: (a->b)->((a,a)->(b,b))
pairMap f (a,b) = (f a, f b)

pairMap2 f = (\(a,b)->(f a, f b))

map2 :: (a->a->b)->[a]->[b]
map2 f l = zipWith f l (drop 1 l)


----- Viernes 15/09 -------

isDigitStr :: String -> Bool
isDigitStr str = foldr(&&) True (map isDigit str)

isDigitStr2 :: String -> Bool
isDigitStr2 str = (length str) == (length (filter isDigit str))

isDigitStr3 :: String -> Bool
isDigitStr3 str = all isDigit str

isDigitStr4 :: String -> Bool
isDigitStr4 (c:cs) = (isDigit c) && (isDigitStr4 cs)
isDigitStr4 [] = True

areDigitStrs :: [String] -> Bool
areDigitStrs strs = all isDigitStr strs

appendLength :: [[Int]] -> [[Int]]
appendLength xs = map (\is -> l:is) xs
   where l = length xs

isSorted :: (Ord a) => [a] -> Bool
isSorted (a:b:xs) = a <= b && isSorted(b:xs)
isSorted [a] = True
isSorted [] = True

manhattanDistance v1 v2
 | length v1 /= length v2 = error "No!"
 | otherwise = sum (zipWith (\x y -> abs(x-y)) v1 v2)

hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = sum(zipWith (\x y -> if x == y then 0 else 1) xs ys) + abs(length xs - length ys)


------- Miércoles 20/09 --------

rmsd :: [Double] -> [Double] -> Double
rmsd a b
    | length a /= length b = error "Error!!"
    | length a == 0 = 0.0
    | otherwise = sqrt((sum(zipWith(\x y -> (x-y) ** 2) a b)) / fromIntegral (length a))


type Function = [Double] -> [Double] -> Double

nearest :: Function -> [Double] -> [[Double]] -> [Double]
nearest fun x ys = n
    where (_,n)= minimum [(fun x y, y) | y <-ys]

greatest, least :: (Ord b, Ord a) => (a -> b) -> [a] -> a
greatest fun xs = n where (_,n) = maximum [(fun y, y) | y<-xs]

least fun xs = n where (_,n) = minimum [(fun y, y) | y<-xs]

greatest2 fun xs = n where (n,_) = maximum [(fun y, y) | y<-xs]
greatests fun xs = map snd (filter (\(x,_) -> x == (greatest2 fun xs)) [(fun y, y) | y<-xs])

least2 fun xs = n where (n,_) = minimum [(fun y, y) | y<-xs]
leasts fun xs = map snd (filter (\(x,_) -> x == (least2 fun xs)) [(fun y, y) | y<-xs])


kNearest fun n xs points
    | n == 0 = []
    | n >= length points = points
    | otherwise = take n (map(\(_,x) -> x ) sortedList)
     where sortedList = sort [(fun xs y, y) | y <- points]
