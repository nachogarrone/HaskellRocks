module Main where

import Contingency
import Data.Random.Extras

emptyBoard = [Vacia,Vacia,Vacia,Vacia,Vacia,
              Vacia,Vacia,Vacia,Vacia,Vacia,
              Vacia,Vacia,Vacia,Vacia,Vacia,
              Vacia,Vacia,Vacia,Vacia,Vacia,
              Vacia,Vacia,Vacia,Vacia,Vacia]

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

main :: IO ()
main = do
--     (putStrLn (showBoard (ContingencyGame emptyBoard constants operators)))
    x <- return (Just (choiceExtract operators))
    print (show x)
