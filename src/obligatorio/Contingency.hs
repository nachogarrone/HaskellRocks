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
data Operador = AND2 | AND3 | OR2 | OR3 | XOR | IFF | IF | NOT | NULO deriving (Eq, Show, Enum)
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
data ContingencyGame = ContingencyGame Tablero (ContingencyPlayer, ContingencyPlayer) deriving (Show)
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
    (p1, p2) <- generatePlayerOperators(PlayerTrue [], PlayerFalse []) operators
    return (ContingencyGame board (p1, p2))

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
actions (ContingencyGame board (PlayerTrue opTrue, PlayerFalse opFalse)) player = do
    let actPlayer = fromJust (activePlayer (ContingencyGame board (PlayerTrue opTrue, PlayerFalse opFalse)))
    if (actPlayer) /= player then []
    else do
        retrievePossibleMoves board player

retrievePossibleMoves :: Tablero -> ContingencyPlayer -> [ContingencyAction]
retrievePossibleMoves board (PlayerTrue []) = []
retrievePossibleMoves board (PlayerFalse []) = []
retrievePossibleMoves board (PlayerTrue opers) = do
    playerOperator <- return (head opers)
    let newOpers = removeItem playerOperator opers
    let operandsNeeded = classifyOperator playerOperator
    retrieveMovesByOperatorType playerOperator operandsNeeded board 8 []

classifyOperator :: Operador -> Int
classifyOperator o
        | (o == AND2) || (o == OR2) || (o == IFF) || (o == IF) || (o == XOR) = 2
        | (o == AND3) || (o == OR3) = 3
        | otherwise = 1

retrieveMovesByOperatorType :: Operador -> Int -> Tablero -> Int -> [ContingencyAction] -> [ContingencyAction]
retrieveMovesByOperatorType _ _ [] _ _ = []
retrieveMovesByOperatorType oper n (x:xs) i actionList 
        | n == 1 =
            if (x == Vacia) then do
                let left = i-1
                let right = i+1
                let up = i-7
                let down = i+7

                if (left >= 7 && left < 40 && ((x:xs) !! left) /= Vacia) then
                    (ContingencyAction oper LEFT left):actionList
                else
                    if (right >= 7 && right <= 41 && ((x:xs) !! right) /= Vacia) then
                        (ContingencyAction oper RIGHT right):actionList
                    else
                        if (up < 0 && ((x:xs) !! up) /= Vacia) then
                            (ContingencyAction oper UP up):actionList
                        else
                            if (down < 48 && ((x:xs) !! down) /= Vacia) then
                                (ContingencyAction oper DOWN down):actionList
                            else
                                retrieveMovesByOperatorType oper n xs (i+1) actionList
            else 
                retrieveMovesByOperatorType oper n xs (i+1) actionList
        
        | n == 2 =
            if (x == Vacia) then do
                let left = i-1
                let right = i+1
                let up = i-7
                let down = i+7

                if ((left >= 7 && left < 40 && ((x:xs) !! left) /= Vacia) 
                    && (right >= 7 && right <= 41 && ((x:xs) !! right) /= Vacia))  then
                        (ContingencyAction oper LEFT left):actionList
                else
                    if ((up < 0 && ((x:xs) !! up) /= Vacia) && (down < 48 && ((x:xs) !! down) /= Vacia)) then
                        (ContingencyAction oper UP up):actionList
                    else
                        retrieveMovesByOperatorType oper n xs (i+1) actionList
            else 
                retrieveMovesByOperatorType oper n xs (i+1) actionList

        | n == 3 =
            if (x == Vacia) then do
                let left = i-1
                let right = i+1
                let up = i-7
                let down = i+7

                if ((left >= 7 && left < 40 && ((x:xs) !! left) /= Vacia) 
                    && (right >= 7 && right <= 41 && ((x:xs) !! right) /= Vacia)
                    && ((up < 0 && ((x:xs) !! up) /= Vacia) || (down < 48 && ((x:xs) !! down) /= Vacia)))  then
                        (ContingencyAction oper LEFT left):actionList
                else 
                    if ((up < 0 && ((x:xs) !! up) /= Vacia) && (down < 48 && ((x:xs) !! down) /= Vacia)
                        && ((left >= 7 && left < 40 && ((x:xs) !! left) /= Vacia)
                        || (right >= 7 && right <= 41 && ((x:xs) !! right) /= Vacia))) then
                            (ContingencyAction oper UP up):actionList
                    else
                        retrieveMovesByOperatorType oper n xs (i+1) actionList
            else 
                retrieveMovesByOperatorType oper n xs (i+1) actionList

        | otherwise = error("Wrong number of operands detected. We can't proccess it.")

nextState :: ContingencyGame -> ContingencyPlayer -> ContingencyAction -> IO ContingencyGame
nextState (ContingencyGame board operators) player (ContingencyAction operator orientation n)
    | (isFinished (ContingencyGame board operators)) = error "Juego ya finalizado!"
    | not (elem (ContingencyAction operator orientation n) (actions (ContingencyGame board operators) player)) = error "La casilla no se puede ubicar ahi!"
    | otherwise = return (ContingencyGame (executeAction board (ContingencyAction operator orientation n)) operators)

executeAction :: Tablero -> ContingencyAction -> Tablero
executeAction board (ContingencyAction operator orientation n) = board
-- executeAction casillas (ContingencyAction operator orientation n) = replace casillas n (operator orientation)

isFinished :: ContingencyGame -> Bool
isFinished (ContingencyGame board operators) = if ((length (filter (== Vacia) board))==4) then True else False


score :: ContingencyGame -> ContingencyPlayer -> Int
score _ _ = error "not implemented"
-- score (ContingencyGame tab (playerT, playerF)) player
--     | player == playerT = (length (filter True tab))
--     | player == playerF = (length (filter False tab))

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
randomAgent game player = let _actions = actions game player in do
                                print player
                                print _actions
                                rnum <- getStdRandom (randomR (0,(length _actions) - 1))
                                print rnum
                                if rnum < 0 then return (ContingencyAction NULO UP 0) else return (_actions !! rnum)



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
    nextAction <- (randomAgent g active)
    nextBoard <- (nextState g active nextAction)
    runMatch players nextBoard

runOnConsole :: IO (Int, Int)
runOnConsole = do
  board <- beginning
  runMatch (PlayerTrue [], PlayerFalse []) board



-- Helpers
pick :: [a] -> IO a
pick xs = do
  index <- randomRIO (0, length xs - 1)
  return  (xs !! index)

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

replace :: [Casilla] -> Int  -> Casilla -> [Casilla]
replace xs n el = take n xs ++ [el] ++ drop (n + 1) xs

enableConstant :: Casilla -> Casilla
enableConstant (Casilla (Constante v False)) = (Casilla (Constante v True))
enableConstant (Casilla (Constante v True)) = (Casilla (Constante v True))
enableConstant c = c

generatePlayerOperators :: (ContingencyPlayer, ContingencyPlayer) -> Operadores -> IO (ContingencyPlayer, ContingencyPlayer)
generatePlayerOperators (a, b) [] = return (a, b)
generatePlayerOperators (PlayerTrue opTrue, PlayerFalse opFalse) o = do
    randomOperatorP1 <-  pick o
    let o2 = removeItem randomOperatorP1 o
    randomOperatorP2 <-  pick o2
    let o3 = removeItem randomOperatorP2 o2
    generatePlayerOperators (PlayerTrue (opTrue ++ [randomOperatorP1]), PlayerFalse (opFalse ++ [randomOperatorP2])) o3
