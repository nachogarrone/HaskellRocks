{-
	Plantilla de codigo para el proyecto del curso 2017 de "Electiva - Programacion Funcional" para
	las carreras de Ingenieria y Licenciatura en Informatica de la FIT (UCU).

	Por Leonardo Val.
-}
module Contingency where

import Data.Maybe
import System.Random
import System.IO.Unsafe

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
data Casilla = Casilla Constante | Operador Orientation | Vacia deriving (Eq)

instance (Show Casilla) where
    show Vacia = "[    ]"
    show (Casilla (Constante v e)) = if e then if v then "[ CT ]" else "[ CF ]" else "[ C? ]"

data Constante = Constante Valor Estado deriving (Eq)
instance (Show Constante) where
    show (Constante v e) = show v


type Valor = Bool
type Estado = Bool

data Orientation = UP | DOWN | LEFT | RIGHT deriving (Eq)

data ContingencyPlayer = PlayerTrue Operadores | PlayerFalse Operadores deriving (Eq, Show)
data ContingencyGame = ContingencyGame Tablero (ContingencyPlayer, ContingencyPlayer)
data ContingencyAction = ContingencyAction Operador Orientation Int deriving (Eq, Show)

instance (Show Orientation) where
    show UP = "ARR"
    show DOWN = "ABA"
    show RIGHT = "DER"
    show LEFT = "IZQ"

-- OESTE

-- beginning y nextState tienen IO porque pueden tener aleatoriedad

beginning :: IO ContingencyGame
beginning = do
    board <- buildBoard constants
    {- TODO: Need to load in every player their operators which were retrieved randomly CHECK generatePlayerOperators
    AND2 and OR2 have to be replaced by a list generated from generatePlayerOperators-}
    return (ContingencyGame board (PlayerTrue [AND2], PlayerFalse [OR2]))

activePlayer :: ContingencyGame -> Maybe ContingencyPlayer
activePlayer (ContingencyGame board (PlayerTrue opT, PlayerFalse opF)) = if (isFinished (ContingencyGame board (PlayerTrue opT, PlayerFalse opF))) then
    Nothing else
        (if ((mod (contarOper board) 2)==0) then
            Just (PlayerTrue opT)
            else Just (PlayerFalse opF))

contarOper :: Tablero -> Int
contarOper tabl = length (filter (filtroOper) tabl)

filtroOper :: Casilla -> Bool
filtroOper (Operador _) = True
filtroOper _ = False

-------------------------------

actions :: ContingencyGame -> ContingencyPlayer -> [ContingencyAction]
actions game player = do
    let x = (activePlayer game)
    if (fromJust x) /= player then []
    else do
        []


nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState (ContingencyGame board operators) player (ContingencyAction operator orientation n)
                                                                | (isFinished (ContingencyGame board operators)) = error "Juego ya finalizado!"
                                                                | not (elem (ContingencyAction operator orientation n) (actions (ContingencyGame board operators) player)) = error "La casilla no se puede ubicar ahi!"
                                                                | otherwise = error "To implement"

-- chequear que la posicion en el tablero esté vacia
executeAction :: Tablero -> ContingencyAction -> Tablero
executeAction board (ContingencyAction operation orientation n) = board

isFinished :: ContingencyGame -> Bool
isFinished (ContingencyGame board operators) = if ((length (filter (== Vacia) board))==4) then True else False


score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "score has not been implemented!" --TODO

showBoard :: ContingencyGame -> String
showBoard (ContingencyGame board _)  = auxTabl board 0
--showBoard _ = error "showBoard has not been implemented!" --TODO

auxTabl :: Tablero -> Int -> String
auxTabl [x] 6 = show x ++ "\n"
auxTabl [x] _ = show x ++ "\n"
auxTabl (x:xs) 6 = (show x ++ "\n") ++ auxTabl xs (0)
auxTabl (x:xs) n = show x ++ auxTabl xs (n+1)


showAction :: ContingencyAction -> String
showAction (ContingencyAction operator orientation n) = (show operator ++ " ") ++ ((show orientation ++ " ") ++ show n)

readAction :: String -> ContingencyAction
readAction _ = error "readAction has not been implemented!" --TODO

-- Match controller --------------------------------------------------------------------------------
type Agent = ContingencyGame -> ContingencyPlayer -> IO ContingencyAction

consoleAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
consoleAgent _ _ = error "consoleAgent has not been implemented!"

randomAgent :: ContingencyGame -> ContingencyPlayer -> IO ContingencyAction
randomAgent _ _ = error "randomAgent has not been implemented!"


--  a1, a5, b2, b4, d2, d4, e1 y e5.
positions = "0TTTTT0T10001FT01010FT00000FT01010FT10001F0FFFFF0"
buildBoard constants = do
    shuffConstants <- (shuffle constants)
    return (mapPosition positions shuffConstants 0)

mapPosition [] _ _ = []
mapPosition (x:xs) c i
    | x == '1' = ((Casilla (c !! i)) : (mapPosition xs c (i+1)))
    | x == '0' = (Vacia: (mapPosition xs c i))
    | x == 'T' = ((Casilla (Constante True True)) : (mapPosition xs c (i)))
    | x == 'F' = ((Casilla (Constante False True)) : (mapPosition xs c (i)))
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
  runMatch (PlayerTrue [], PlayerFalse []) board



-- Helpers
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

removeItem _ [a] = [a]
removeItem x (y:ys)
    | x == y    = ys
    | otherwise = y : removeItem x ys

insertAt l y i
    | ((length l) == 0) && (i == 0) = [y]
    | (length l) < i = error "index out of range"
    | otherwise = (take i l)++[y]++(drop i l)

replace :: [Casilla] -> Int  -> Casilla -> [Casilla]
replace xs n el = take n xs ++ [el] ++ drop (n + 1) xs

enableConstant :: Casilla -> Casilla
enableConstant (Casilla (Constante v False)) = (Casilla (Constante v True))
enableConstant (Casilla (Constante v True)) = (Casilla (Constante v True))
enableConstant c = c

generatePlayerOperators :: (ContingencyPlayer, ContingencyPlayer) -> Operadores -> (ContingencyPlayer, ContingencyPlayer)
generatePlayerOperators (a, b) [] = (a, b)
generatePlayerOperators (PlayerTrue opTrue, PlayerFalse opFalse) o = do
    let randomOperatorT = unsafePerformIO (pick o)
    let randomOperatorF = unsafePerformIO (pick o)
    (PlayerTrue (randomOperatorT:opTrue), PlayerFalse (randomOperatorF:opFalse))
    -- let o2 = removeItem t o
    -- f <- (return (pick o2))
    -- let o3 = removeItem f o2
    -- generatePlayerOperators (PlayerTrue (t:opTrue), PlayerFalse (f:opFalse)) o3