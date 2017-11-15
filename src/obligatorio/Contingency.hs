{-
	Plantilla de codigo para el proyecto del curso 2017 de "Electiva - Programacion Funcional" para
	las carreras de Ingenieria y Licenciatura en Informatica de la FIT (UCU).

	Por Leonardo Val.
-}
module Contingency where

import Data.Maybe


-- Game logic stub ---------------------------------------------------------------------------------

type Tablero = [Casilla]
type Constantes = [Constante]
type Operadores = [Operador]
data Operador = AND2 | AND3 | OR2 | OR3 | XOR | IFF | IF | NOT
data Casilla = Casilla Constante | Operador Posicion | Vacia

instance (Show Casilla) where
    show Vacia = "[ ]"
    show (Casilla (Constante v e)) = if e then if v then "[ CT ]" else "[ CF ]" else "[ C? ]"

data Constante = Constante Valor Estado

type Valor = Bool
type Estado = Bool

data Posicion = Arriba | Abajo | Izquierda | Derecha

data ContingencyPlayer = PlayerTrue | PlayerFalse deriving (Eq, Show, Enum)
data ContingencyGame = ContingencyGame Tablero Constantes Operadores
data ContingencyAction

-- OESTE

-- beginning y nextState tienen IO porque pueden tener aleatoriedad

beginning :: IO ContingencyGame
beginning = error "beginning has not been implemented!" --TODO

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer _ = error "activePlayer has not been implemented!" --TODO

actions :: ContingencyGame -> ContingencyPlayer -> [ContingencyAction]
actions _ _ = error "actions has not been implemented!" --TODO

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState _ _ _ = error "nextState has not been implemented!" --TODO

isFinished :: ContingencyGame -> Bool
isFinished _ = error "isFinished has not been implemented!" --TODO

score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "score has not been implemented!" --TODO

showBoard :: ContingencyGame -> String
showBoard (ContingencyGame tablero _ _)  = auxTabl tablero 0
--showBoard _ = error "showBoard has not been implemented!" --TODO

auxTabl :: Tablero -> Int -> String
auxTabl [x] 4 = show x ++ "\n"
auxTabl [x] _ = show x ++ "\n"
auxTabl (x:xs) 4 = (show x ++ "\n") ++ auxTabl xs (0)
auxTabl (x:xs) n = show x ++ auxTabl xs (n+1)


showAction :: ContingencyAction -> String
showAction _ = error "showAction has not been implemented!" --TODO

readAction :: String -> ContingencyAction
readAction _ = error "readAction has not been implemented!" --TODO

-- Match controller --------------------------------------------------------------------------------
type Agent = ContingencyGame -> ContingencyPlayer -> IO ContingencyAction

consoleAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
consoleAgent _ _ = error "consoleAgent has not been implemented!"

randomAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
randomAgent _ _ = error "randomAgent has not been implemented!"

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
