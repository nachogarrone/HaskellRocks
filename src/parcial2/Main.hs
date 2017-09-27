module Main where

import Algebraicos

main :: IO ()
main = do
--    print (monthDays Feb 2017)
--    print (monthDays Feb 2016)
--    print (monthDays Feb 2015)
    print(walk2 (0,0) [StepLeft 3])
    print(walk2 (0,0) [StepRight 2])
    print(walk2 (0,0) [StepLeft 3])
    print(walk2 (0,0) [StepLeft 2, StepUp 3])
--     print (gameRPS Papel Tijera)
--     print (gameRPS (Papel, Piedra) (Papel, Papel))
--     print (gameRPS (Papel, Piedra) (Papel, Tijera))
--     print (gameRPS (Papel, Tijera) (Papel, Tijera))
--     print (monthDays Feb 2017)
--     print (monthDays Feb 2016)
--     print (monthDays Feb 2015)
