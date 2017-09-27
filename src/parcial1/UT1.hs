{-Gabriel Camaño, Ignacio Fernández, Fernando Torterolo-}
module UT1 where
{-

potencia :: Float -> Int -> Float
potencia x e = 
    if (e < 1)
    then (1 / potencia x (-e))
    else
        if (e == 0)
        then 1
        else
            if ((x `mod` 2) == 0)
            then ((potencia x (e `div` 2)) * (potencia x (e `div` 2))) 
            else (x * (potencia x (e-1)))

-}
import Data.List
import Data.Char

median :: Int->Int->Int->Int
median x y z = (sort [x,y,z])!!1

isDigit :: Char->Bool
isDigit c = (let v = ord c in (v >= 48) && (v <= 57))

isDigit2:: Char -> Bool
isDigit2 c = elem c ['0'..'9']

isDigit3::Char->Bool
isDigit3 c = c >= '0' && c <= '9'

base2 :: Int->String
base2 n = if(n == 0) then "0" 
          else if(n==1) then "1" 
                else if (n<0) then error "error!"
                    else (base2 (n`div`2))++(show (n`mod`2))

linea :: Int->Char->String
linea i c
    | i==0 = "\n"
    | otherwise = [c] ++ (linea (i-1) c)

stairs:: Int->Char->String
stairs i c
   | i == 0 = ""
   | otherwise = (stairs (i-1) c) ++ (linea i c)

get16Digit::Int->String
get16Digit d 
    | (d >= 0 && d <= 9) = show(d)
    | d == 10 = "A"
    | d == 11 = "B"
    | d == 12 = "C"
    | d == 13 = "D"
    | d == 14 = "E"
    | d == 15 = "F"
    | otherwise = error "getDigit16:not a hex digit"

base16:: Int->String
base16 n 
    | n < 16 = get16Digit n
    | n < 0 = error "error!"
    | otherwise = base16 (n`div`16) ++ get16Digit (n`mod`16)

collatz::Int->Int
collatz n
    | n < 1 = 0
    | n == 1 = 1
    | (n`mod`2 == 0) = 1 + collatz(n`div`2)
    | otherwise = 1 + collatz(3 * n + 1)


closestPower:: Float->Float->String
closestPower b n = let xp = truncate (logBase b n) in if(abs(n-b^xp) <= abs(n-b^(xp+1))) then show xp else show (xp+1)

aux :: Int -> Int -> Bool
aux n f  
    | f == 1 = True
    | (n`mod`f) == 0 = False
    | otherwise = aux n (f-1)

isPrime :: Int -> Bool
isPrime n 
    | n == 1 = False
    | n == 0 = False
    | n < 0 = error "isPrime:not a natural number"
    | otherwise = aux n (n-1)