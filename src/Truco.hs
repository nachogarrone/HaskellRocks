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

_ptosEnvido2 [(a1,a2), (b1,b2), (c1,c2)] = if a1 == b1 then (valorEnvido a2)+(valorEnvido b2)+20 else if a1==c1 then (valorEnvido a2)+(valorEnvido c2)+20 else (valorEnvido b2)+(valorEnvido c2)+20

_ptosEnvido1 [(a1,a2), (b1,b2), (c1,c2)] = maximum [valorEnvido a2, valorEnvido b2, valorEnvido c2]


valorEnvido n = if n>=10 then 0 else n

ordNaipe a b = valorCarta a - valorCarta b


-- oro copa espada basto

valorCarta (2,1) = 20
valorCarta (3,1) = 19
valorCarta (2,7) = 18
valorCarta (0,7) = 17
valorCarta (_,3) = 16
valorCarta (_,2) = 15
valorCarta (_,1) = 14
valorCarta (_,a) = a
