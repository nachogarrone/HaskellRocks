module Main where

import TicTacToe

main :: IO ()
main = do
--     print (finished ".........")
--     print (finished "XOOOXX..X")
--     print (finished "OXOOXXO.X")
--     print (finished "OXOOXXOOX")
--     print (activePlayer ".........")
--     print (activePlayer "..OOXX...")
--     print (activePlayer "..OOXX..X")
--     print (activePlayer "OXOOXXOXX")
--     print (moves ".........")
    print (next "........." 5)
