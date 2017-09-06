module Truco where

-- Definir la función esFlor que toma una lista de naipes y chequee si ñas cartas forman una
-- flor, de acuerdo con las reglas del Truco (argentino).

type Naipe = (Int, Int)
esFlor :: [Naipe] -> Bool

esFlor [n1, n2, n3] | n1 == n2 || n2 == n3 || n1 == n3 = error "!!"
esFlor ([(a,_), (b,_), (c,_)]) = a == b && a == c
esFlor _ = False


ptosEnvido [n1,n2,n3] | esFlor [n1,n2,n3] = error "Tenés flor panqueque!"
ptosEnvido ([(a1,a2), (b1,b2), (c1,c2)]) = if a1 == b1 || a1 == c1 || b1 == c1 then _ptosEnvido2 [(a1,a2), (b1,b2), (c1,c2)] else _ptosEnvido1 [(a1,a2), (b1,b2), (c1,c2)]
ptosEnvio _ = error "error"

_ptosEnvido2 [(a1,a2), (b1,b2), (c1,c2)] = if a1 == b1 then (valorCarta a2)+(valorCarta b2)+20 else if a1==c1 then (valorCarta a2)+(valorCarta c2)+20 else (valorCarta b2)+(valorCarta c2)+20

_ptosEnvido1 [(a1,a2), (b1,b2), (c1,c2)] = maximum [valorCarta a2, valorCarta b2, valorCarta c2]

valorCarta n = if n>=10 then 0 else n
