module TicTacToe where
import Data.List
    
type TicTacToe = String

start:: TicTacToe
start = replicate 9 '.'

finished::TicTacToe -> Bool
finished ['X','X','X',_,_,_,_,_,_] = True
finished [_,_,_,'X','X','X',_,_,_] = True
finished [_,_,_,_,_,_,'X','X','X'] = True
finished ['X',_,_,'X',_,_,'X',_,_] = True
finished [_,'X',_,_,'X',_,_,'X',_] = True
finished [_,_,'X',_,_,'X',_,_,'X'] = True
finished ['X',_,_,_,'X',_,_,_,'X'] = True
finished [_,_,'X',_,'X',_,'X',_,_] = True
finished ['O','O','O',_,_,_,_,_,_] = True
finished [_,_,_,'O','O','O',_,_,_] = True
finished [_,_,_,_,_,_,'O','O','O'] = True
finished ['O',_,_,'O',_,_,'O',_,_] = True
finished [_,'O',_,_,'O',_,_,'O',_] = True
finished [_,_,'O',_,_,'O',_,_,'O'] = True
finished ['O',_,_,_,'O',_,_,_,'O'] = True
finished [_,_,'O',_,'O',_,'O',_,_] = True
finished t = not (elem '.' t)

turno::TicTacToe->Char
turno t 
   | (length [x | x <- t,x=='X']) > (length [x | x <- t,x=='O']) = 'O'
   | otherwise = 'X'

moves::TicTacToe->(Char,[Int])
moves t 
    | finished t = (turno t,[])
    | otherwise = (turno t, [i | i <-[0..8], t!!i=='.'])

-- format a game board for on-screen printing
-- La informacion que se le pase a esta funcion es la impresa en el tablero
-- putStr (showGame start) probalo de esa forma
showGame :: String -> String
showGame [a,b,c,d,e,f,g,h,i] =
        emptyRow ++
        "|    | 1 | 2 | 3 |\n" ++
        emptyRow ++
        row "1" [[a],[b],[c]] ++
        row "2" [[d],[e],[f]] ++
        row "3" [[g],[h],[i]]
    where
        emptyRow = "+----+---+---+---+\n"
        row n x = "| " ++ n ++ "  | " ++ intercalate " | " x ++ " |\n" ++ emptyRow
 
{- 
012
345
678
-}
