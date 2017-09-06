module Truco where

-- Definir la función esFlor que toma una lista de naipes y chequee si ñas cartas forman una
-- flor, de acuerdo con las reglas del Truco (argentino).

type Naipe = (Int, Int)
esFlor :: [Naipe] -> Bool

esFlor [n1, n2, n3] | n1 == n2 || n2 == n3 || n1 == n3 = error "!!"
esFlor ([(a,_), (b,_), (c,_)]) = a == b && a == c
esFlor _ = False
