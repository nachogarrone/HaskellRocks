import Data.Char
import Data.List

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