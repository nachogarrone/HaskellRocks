module Prepa where

import Data.List

type Naipe = (Int, Int)

paloOro = 0
paloCopa = 1
paloEspada = 2
paloBasto = 3

esFlor :: [Naipe] -> Bool
esFlor [n1, n2, n3] | n1 == n2 || n2 == n3 || n1 == n3 = error "!!"
esFlor ([(a,_), (b,_), (c,_)]) = a == b && a == c
esFlor _ = False

estanEnOrden :: [Int] -> Bool
estanEnOrden xs
    | length xs /= 3 || length xs == 0 = error "Cantidad de cartas incorrecta"
    | otherwise = xs!!2 == xs !! 1 + 1 && xs!!1 == xs!!0 + 1

isStraight :: [Naipe] -> Bool
isStraight xs
    | esFlor xs = esFlor xs && estanEnOrden ys
    | otherwise = False
    where ys = sort [y | (_, y) <- xs]

esNaipeValido :: Naipe -> Bool
esNaipeValido naipe
    | (fst naipe == paloOro || fst naipe == paloCopa ||
        fst naipe == paloEspada || fst naipe == paloBasto) && elem (snd naipe) [1..12] = True
    | otherwise = False

-- No anda!
distincts :: [Int] -> [Int]
distincts [x] = [x]
distincts (x:xs)
    | notElem x y = x:y distincts xs
    | otherwise = distincts xs
    where y = []
