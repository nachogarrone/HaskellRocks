module TicTacToe where
import Data.List

next s p = if ((length ms) == 0) || (not (elem p ms)) then error "Terminó!!" else (splice s p 1 [ap]) where (ap, ms) = moves s

moves s = if (finished s) then error "Ya terminó!!" else (activePlayer s, elemIndices '.' s)

activePlayer s = if (finished s) then error "Ya terminó!!" else (if even (length (elemIndices 'X' s)) then 'X' else '0')

finished s = not (elem '.' s) || rowWin s 'X' || rowWin s 'O' || colWin s 'X' || colWin s 'O' || diaWin s 'X' || diaWin s 'O'

rowWin [a,b,c,_,_,_,_,_,_] p = a == p && b == p && c == p
rowWin [_,_,_,a,b,c,_,_,_] p = a == p && b == p && c == p
rowWin [_,_,_,_,_,_,a,b,c] p = a == p && b == p && c == p

colWin [a,_,_,b,_,_,c,_,_] p = a == p && b == p && c == p
colWin [_,a,_,_,b,_,_,c,_] p = a == p && b == p && c == p
colWin [_,_,a,_,_,b,_,_,c] p = a == p && b == p && c == p

diaWin [a,_,_,_,b,_,_,_,c] p = a == p && b == p && c == p
diaWin [_,_,a,_,b,_,c,_,_] p = a == p && b == p && c == p


splice s 0 0 t = t ++ s
splice (c:cs) 0 d t = splice cs 0 (d-1) t
splice (c:cs) p d t = c:(splice cs (p-1) d t)
