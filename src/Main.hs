module Main where

import Data.List
import Prelude

main :: IO ()
-- factorial n = if n == 0 then 1 else n * factorial (n - 1)
-- promptLine :: String -> IO String
-- promptLine prompt = do
--     putStrLn prompt
--     getLine
--
-- rule3 a b c = (b * c) / a
--
-- roundP f n = (fromInteger $ round $ f * (10**n)) / (10.0**n)

-- median a b c = let o = sort ([a, b, c]) in o !!1

-- overlap a b c d = let((_,x),(y,_)) = if a < c then ((a,b),(c,d)) else ((c,d),(a,b)) in x >= y

-- clamp a b x = if x < a then a else if x > b then b else x

-- isDigit s = elem s ['0'..'9']

-- acu x t = if t then x+1 else x-1
--
-- filterParen :: String -> String
-- filterParen ('(':s) = '(':(filterParen s)
-- filterParen (')':s) = ')':(filterParen s)
-- filterParen (_:s) = filterParen s
-- filterParen "" = ""
--
-- checkParen s = _checkParen s 0
-- _checkParen ('(':s) c = _checkParen s (acu c True)
-- _checkParen (')':s) c = _checkParen s (acu c False)
-- _checkParen (_:s) c = _checkParen s c
-- _checkParen "" c = c==0
--
-- collatz n = if n==1 then 1 else if (mod n 2) == 0 then 1+(collatz (div n 2)) else 1+(collatz (3*n+1))

-- _closestPower :: Int -> Int -> Int
-- closestPower b r = if b>0 && r>0 then (_closestPower b r) else error "error!"
-- _closestPower b r = let x = (logBase (fromIntegral b) (fromIntegral r)) in let f = (b ^ (floor x)) in let c = (b ^ (ceiling x)) in if (abs (r-c))<(abs (r-f)) then (ceiling x) else (floor x)

-- isPalindrome :: Int -> Bool
-- isPalindrome n = let s = show n in s == (reverse s)

-- bar n m = let h = replicate n '#' in if (length h) <= m then (addSpace (m-(length h)) h) else (take m h)
-- addSpace 0 s = s
-- addSpace n s = addSpace (n-1) (' ':s)

getIdx []

main
--     print (bar 3 5)
--     print (bar 0 5)
--     print (bar 10 5)
--     print (bar 0 0)
--     print (bar 3 3)
--     print (isPalindrome 0)
--     print (isPalindrome 12)
--     print (isPalindrome 121)
--     print (isPalindrome 11)
--     print (isPalindrome 1212)
--     print (closestPower 6 21)
--     print (closestPower 6 22)
--     print (closestPower 2 14)
--     print (closestPower 0 3)
--     print (collatz 3)
--     print (checkParen "(h(o)la)")
--     print (isDigit 'a')
--     print (isDigit '1')
--     print (clamp 5 7 1)
--     print (clamp 5 7 6)
--     print (clamp 5 7 8)
--     print (clamp 5 7 5)
--     print (clamp (-1) 0 2)
--     print (clamp (-1) 0 (-7))
--     print (clamp 0 0 0)
--     print (clamp 0 1 2)
--     print (overlap 1 3 2 4)
--     print (overlap (-1) 8 2 4)
--     print (overlap 2 4 1 3)
--     print (overlap 1 2 3 4)
--     print (overlap 0 1 0 1)
--     print (overlap 0 0 0 0)
--     name <- promptLine "Como te llamas?"
--     putStrLn ("Hola " ++ name)
--     print (5+5)
--     print (rule3 1 10 7)
--     print (roundP 126.126 1)
--     print (roundP 126.126 2)
--     print (roundP 126.126 3)
--     print (roundP 126.126 5)
--     print (roundP 126.126 0)
--     print (roundP 126.126 (-1))
--     print (roundP 126.126 (-2))
--     print (roundP 126.126 (-3))
