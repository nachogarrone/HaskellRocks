module Main where

-- import TicTacToe
-- import Truco
-- import Entorno
-- import AltoOrden
import Prepa

points = [[1,3],[2,4]]

main :: IO ()
main = do
    print(distincts [1,1,1,1,2,2,2,2,2,2,3,3,3,4,5,6,6,7])
--     print(esNaipeValido (1,3))
--     print(esNaipeValido (1,12))
--     print(esNaipeValido (1,13))
--     print(esNaipeValido (5,1))
--    print(isStraight [(1,2), (1,3), (1,4)])
--    print(isStraight [(1,5), (1,3), (1,4)])
--    print(isStraight [(1,2), (2,3), (1,4)])
--    print (kNearest rmsd 2 [1,2] points)
--     print (greatests (^2) [-2..2])
--     print (leasts (^2) [-2..2])
--     print (greatests ((flip mod) 2) [0..5])
--     print (leasts ((flip mod) 3) [0..5])
--     print (least (^2) [-2..1])
--     print (greatest show [-20..20])
--     print (least show [-20..20])
--     print (nearest rmsd [1.0,2.0] [[1.0,3.0],[2.0,4.0]])
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
--     print (ptosEnvido [(1,1),(2,2),(1,3)])
--     print (ptosEnvido [(1,1),(1,2),(1,3)])
--     print (ordNaipe (2,1) (3,1))
--     print (ordNaipe (0,7) (2,7))
--     print (ordNaipe (1,3) (3,3))
--    print (ganaTruco [(0,4),(1,5),(2,7)] [(2,3),(1,2),(0,4)])
--     print (ganaTruco [(2,7),(0,4),(1,5)] [(2,3),(1,2),(0,4)])
--     print(entorno [1])
--     print(entorno [0, 1])
--     print(entorno [0, 0, 0])
