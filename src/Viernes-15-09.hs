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