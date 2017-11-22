{-
	Plantilla de codigo para el proyecto del curso 2017 de "Electiva - Programacion Funcional" para
	las carreras de Ingenieria y Licenciatura en Informatica de la FIT (UCU).

	Por Leonardo Val.
-}
module Contingency where

import Data.Maybe
import System.Random


constants = [(Constante True False),(Constante True False),(Constante True False),(Constante True False),
             (Constante False False),(Constante False False),(Constante False False),(Constante False False)]


operators = [AND2,AND2,AND2,AND2,AND2,AND2, -- 6x AND2
             OR2,OR2,OR2,OR2,OR2,OR2, -- 6x OR2
             AND3,AND3,AND3, -- 3x AND3
             OR3,OR3,OR3, -- 3x OR3
             XOR,XOR,XOR,XOR, -- 4x XOR
             IFF,IFF,IFF,IFF, -- 4x IFF
             IF,IF, -- 2x IF
             NOT,NOT -- 2x NOT
             ]

-- Game logic stub ---------------------------------------------------------------------------------

type Tablero = [Casilla]
type Constantes = [Constante]
type Operadores = [Operador]
data Operador = AND2 | AND3 | OR2 | OR3 | XOR | IFF | IF | NOT deriving (Eq, Show, Enum)
data Casilla = Casilla Constante | Operador Posicion | Vacia deriving (Eq)

instance (Show Casilla) where
    show Vacia = "[ ]"
    show (Casilla (Constante v e)) = if e then if v then "[ CT ]" else "[ CF ]" else "[ C? ]"

data Constante = Constante Valor Estado deriving (Eq)
instance (Show Constante) where
    show (Constante v e) = show v


type Valor = Bool
type Estado = Bool

data Posicion = Arriba | Abajo | Izquierda | Derecha deriving (Eq)

data ContingencyPlayer = PlayerTrue | PlayerFalse deriving (Eq, Show, Enum)
data ContingencyGame = ContingencyGame Tablero Operadores
data ContingencyAction = ContingencyAction Operador Posicion Int deriving (Show)

instance (Show Posicion) where
    show Arriba = "Arriba"
    show Abajo = "Abajo"
    show Derecha = "Derecha"
    show Izquierda = "Izquierda"

-- OESTE

-- beginning y nextState tienen IO porque pueden tener aleatoriedad

beginning :: IO ContingencyGame
beginning = do
    board <- buildBoard constants
--     print board
    return (ContingencyGame board operators)

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer (ContingencyGame board operators) = if (isFinished (ContingencyGame board operators)) then Nothing else (if ((mod (contarOper board) 2)==0) then Just PlayerTrue else Just PlayerFalse)

contarOper :: Tablero -> Int
contarOper tabl = length (filter (filtroOper) tabl)

filtroOper :: Casilla -> Bool
filtroOper (Operador _) = True
filtroOper _ = False

-------------------------------

actions :: ContingencyGame -> ContingencyPlayer -> [ContingencyAction]
actions _ _ = error "actions has not been implemented!" --TODO

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState _ _ _ = error "nextState has not been implemented!" --TODO

isFinished :: ContingencyGame -> Bool
isFinished (ContingencyGame board operators) = if ((length (filter (== Vacia) board))==4) then True else False


score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "score has not been implemented!" --TODO

showBoard :: ContingencyGame -> String
showBoard (ContingencyGame tablero _)  = auxTabl tablero 0
--showBoard _ = error "showBoard has not been implemented!" --TODO

auxTabl :: Tablero -> Int -> String
auxTabl [x] 4 = show x ++ "\n"
auxTabl [x] _ = show x ++ "\n"
auxTabl (x:xs) 4 = (show x ++ "\n") ++ auxTabl xs (0)
auxTabl (x:xs) n = show x ++ auxTabl xs (n+1)


showAction :: ContingencyAction -> String
showAction (ContingencyAction operator position n) = (show operator ++ " ") ++ ((show position ++ " ") ++ show n)

readAction :: String -> ContingencyAction
readAction _ = error "readAction has not been implemented!" --TODO

-- Match controller --------------------------------------------------------------------------------
type Agent = ContingencyGame -> ContingencyPlayer -> IO ContingencyAction

consoleAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
consoleAgent _ _ = error "consoleAgent has not been implemented!"

randomAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
randomAgent _ _ = error "randomAgent has not been implemented!"


--  a1, a5, b2, b4, d2, d4, e1 y e5.
positions = "1000101010000000101010001"
buildBoard constants = do
    shuffConstants <- (shuffle constants)
    return (mapPosition positions shuffConstants 0)

mapPosition [] _ _ = []
mapPosition (x:xs) c i
    | x == '1' = ((Casilla (c !! i)) : (mapPosition xs c (i+1)))
    | x == '0' = (Vacia: (mapPosition xs c i))
    | otherwise = error("Wrong cell detected")

runMatch :: (ContingencyPlayer, ContingencyPlayer) -> ContingencyGame -> IO (Int, Int)
runMatch players@(agTrue, agFalse) g = do
  putStrLn (showBoard g)
  if
    (isFinished g)
  then
    return (score g agTrue, score g agFalse)
  else do
    let active = fromJust (activePlayer g)
    nextAction <- (consoleAgent g active)
    nextBoard <- (nextState g active nextAction)
    runMatch players nextBoard

runOnConsole :: IO (Int, Int)
runOnConsole = do
  board <- beginning
  runMatch (PlayerTrue, PlayerFalse) board



-- AUXILIARES
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

removeItem _ [] = []
removeItem x (y:ys)
    | x == y    = ys
    | otherwise = y : removeItem x ys

insertAt l y i
    | ((length l) == 0) && (i == 0) = [y]
    | (length l) < i = error "index out of range"
    | otherwise = (take i l)++[y]++(drop i l)
