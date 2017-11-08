module Parcial2 where

import Data.Maybe


readOneOf :: [String] -> IO (Maybe String)
readOneOf [] = error "Lista vacia!!"
readOneOf p = do
   line <- getLine
   if elem line p then return (Just line) else readOneOf p



data Naipe = Oros Int | Copas Int | Espadas Int | Bastos Int deriving (Eq, Show)

-- cada as vale 11 puntos,
-- cada tres vale 10,
-- cada rey vale 4,
-- cada caballo vale 3
-- cada sota vale 2.
-- El resto de las cartas no valen.
--
-- Definir la función puntajeTute, que calcula el puntaje de un conjunto de naipes.

puntajeTute :: [Naipe] -> Int
puntajeTute [] = 0
puntajeTute (n:ns) = valorCarta n + puntajeTute ns

valorCarta :: Naipe -> Int
valorCarta (Oros 1) = 11
valorCarta (Oros 3) = 10
valorCarta (Oros 12) = 3
valorCarta (Oros 11) = 3
valorCarta (Oros 10) = 2
valorCarta (Oros _) = 0

valorCarta (Copas 1) = 11
valorCarta (Copas 3) = 10
valorCarta (Copas 12) = 3
valorCarta (Copas 11) = 3
valorCarta (Copas 10) = 2
valorCarta (Copas _) = 0

valorCarta (Espadas 1) = 11
valorCarta (Espadas 3) = 10
valorCarta (Espadas 12) = 3
valorCarta (Espadas 11) = 3
valorCarta (Espadas 10) = 2
valorCarta (Espadas _) = 0

valorCarta (Bastos 1) = 11
valorCarta (Bastos 3) = 10
valorCarta (Bastos 12) = 3
valorCarta (Bastos 11) = 3
valorCarta (Bastos 10) = 2
valorCarta (Bastos _) = 0


------------------------------------------------------------------------

-- un punto por cada casilla vacía,
-- P para cada peón,
-- C para cada caballo,
-- A para cada alfil,
-- T para cada torre,
-- D para cada dama y
-- R para cada rey.
-- Las blancas llevan letras mayúsculas y
-- Las negras letras minúscula

data Rol = Blancas | Negras
data Casilla = Vacia | Peon Rol | Caballo Rol | Alfil Rol | Torre Rol | Reina Rol | Rey Rol
type Tablero = [Casilla]

showTablero [] = ""
showTablero (c:cs) = (show c) ++ (showTablero cs)

instance (Show Casilla) where
    show (Vacia) = "."
    show (Peon Blancas) = "P"
    show (Peon Negras) = "p"
    show (Caballo Blancas) = "C"
    show (Caballo Negras) = "c"
    show (Alfil Blancas) = "A"
    show (Alfil Negras) = "a"
    show (Torre Blancas) = "T"
    show (Torre Negras) = "t"
    show (Rey Blancas) = "R"
    show (Rey Negras) = "r"
    show (Reina Blancas) = "D"
    show (Reina Negras) = "d"


--
-- data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty deriving (Eq)
-- instance (Show a) => Show (BinTree a) where
--     show Empty = "_"
--     show (Leaf v) = show v
--     show (Node v ti td) = "(" ++ show v ++ " " ++ show ti ++ " " ++  show td ++ ")"

