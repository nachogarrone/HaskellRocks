module Main where

-- import TicTacToe
import Truco

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
--     print (next "........." 5)
    print (ptosEnvido [(1,1),(2,2),(1,3)])
    print (ptosEnvido [(1,1),(1,2),(1,3)])
