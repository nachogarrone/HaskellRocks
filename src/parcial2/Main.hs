module Main where

import Text.Show.Functions
import CodeAM

main :: IO ()
main = do
    print (evalAM [ConstTrue] ([], emptyState))
--     print (evalAExp (Num 4) emptyState)
--     print (evalAExp (Add (Num 4) (Num 1)) emptyState)
--     print (evalAExp (Mult (Num 4) (Num 1)) emptyState)
--     print (evalAExp (Sub (Num 4) (Num 1)) emptyState)
--     print (evalAExp (Div (Num 4) (Num 1)) emptyState)
--     print (evalAExp (Var "x") emptyState)
--     print (show Empty)
--     print ((Leaf 7))
--     print (Feet 1.0 > Inches 12.0)
--     print (Feet 2.0 > Inches 12.0)
--     print (Feet 1.0 == Inches 12.0)
--     print (Yards 1.0 == Feet 3.0)
--     print (Inches 72.36 == Yards 2.01)
--     print (RED == RED)
--     print (RED == GREEN)
--     print (addOrderedTree Empty 2)
--     print (addOrderedTree (Leaf 2) 3)
--     print (addOrderedTree (Leaf 2) 1)
--     print (replaceBinTree (Leaf 2) 3 Empty)
--     print (replaceBinTree (Leaf 2) 2 Empty)
--     print (isOrderedTree (Node 2 (Leaf 1) (Leaf 3)))
--     print (isOrderedTree (Node 1 (Leaf 2) (Leaf 3)))
--     print (binTreeSearch (Node 2 (Leaf 1) (Leaf 3)) 1)
--     print (binTreeSearch (Node 2 (Leaf 1) (Leaf 3)) 4)
--     print (preorder (Node 7 (Leaf 3) (Node 9 Empty Empty)))
--     print (inorder (Node 7 (Leaf 3) (Leaf 9)))
--     print (postorder (Node 7 (Leaf 3) (Leaf 9)))
--     print (size (Folder "a" []))
--     print (size (File "a.txt" 120))
--     print (size (Folder "a" [Folder "b" []]))
--     print (size (Folder "a" [File "b.txt" 20]))
--     print (turnoEscopa15 [(Oros 7), (Bastos 1), (Bastos 3)] (Espadas 3))
--     print (toRGB GREEN)
--     print (toRGB WHITE)
--     print (toRGB (RGB 1 2 3))
--     print (toInches (Feet 1.0))
--     print (toFeet (Yards 1.0))
--     print (toYards (Inches 72.36))
--     print (toInches (Inches 2.34))
--     print (toFeet (Inches (-12.0)))
--     print (toYards (Feet 0.0))
--     print(addNum (IntNum 12) (IntNum 34))
--     print(addNum (DblNum 1.2) (DblNum 3.4))
--     print(addNum (DblNum 1.2) (IntNum 34))
--     print(addNum (IntNum 12) (DblNum 3.4))
--     print (toInches (Feet 1.0))
--     print (toFeet (Yards 1.0))
--     print (toYards (Inches 72.36))
--     print (toInches (Inches 2.34))
--     print (toFeet (Inches (-12.0)))
--     print (toYards (Feet 0.0))
--    print (monthDays Feb 2017)
--    print (monthDays Feb 2016)
--    print (monthDays Feb 2015)
--     print(walk2 (0,0) [StepLeft 3])
--     print(walk2 (0,0) [StepRight 2])
--     print(walk2 (0,0) [StepLeft 3])
--     print(walk2 (0,0) [StepLeft 2, StepUp 3])
--     print (gameRPS Papel Tijera)
--     print (gameRPS (Papel, Piedra) (Papel, Papel))
--     print (gameRPS (Papel, Piedra) (Papel, Tijera))
--     print (gameRPS (Papel, Tijera) (Papel, Tijera))
--     print (monthDays Feb 2017)
--     print (monthDays Feb 2016)
--     print (monthDays Feb 2015)
