module HexGame where

import Data.List
import Data.Maybe

-----------------------------------------------------------

type HexBoard = [[HexPlayer]]
type HexPlayer = Char
type HexMove = (Int, Int)

-----------------------------------------------------------
-----------------------------------------------------------

activePlayer :: HexBoard -> Maybe HexPlayer
activePlayer board = if (mod (sum (map length (map filter (== 'e') board))) 2 == 0) then 'W' else 'B'

-- filtramos las n
-- aplicamos map para obtener todas las listas de fitler
-- aplicamos map para obtener los largos de todas las listas
-- sumamos los length

-----------------------------------------------------------

isFinished :: HexBoard -> Bool
isFinished board = (winner board)


-----------------------------------------------------------

moves :: HexBoard -> HexPlayer -> [HexMove]
moves board player
    | player /= (activePlayer board) = []
    | otherwise = [(x,y) | x <- 0...10, y <- 0...10, (board !! x) !! y == 'e']

-----------------------------------------------------------

nextBoard :: HexBoard -> HexPlayer -> HexMove -> HexBoard


-----------------------------------------------------------

winner :: HexBoard -> Maybe HexPlayer


-----------------------------------------------------------

startBoard :: HexBoard
startBoard = [["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"],["eeeeeeeeeee"]]

-----------------------------------------------------------

toString :: HexBoard -> String


-----------------------------------------------------------
