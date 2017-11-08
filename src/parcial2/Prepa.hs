module Prepa where

import Data.Maybe

-- Definir el tipo Viaje como un tipo data de Haskell, que permita representar si una persona se movió de un punto a otro de la
-- ciudad caminando, en auto o en omnibus. De ser en omnibus, se debe incluir la línea que se tomó (e.g. 104, 316, etc).
-- Definir una función obtenga el número de línea de un valor de tipo Viaje, retornando Nothing si éste no aplica.
-- nroLinea :: Viaje -> Maybe String

data Viaje = Camina | Auto | Omnibus String deriving (Eq, Show)

nroLinea :: Viaje -> Maybe String
nroLinea (Omnibus x) =  Just x
nroLinea _ = Nothing


-- Implementar en Haskell la función readUntil que lee de la entrada estándar líneas de texto hasta que una de estas líneas
-- cumple con un predicado dado. Por ejemplo:
-- readUntil (== "stop")
-- > uno
-- > dos
-- >
-- > stop
-- = ["uno", "dos", ""]
readUntil:: String -> IO [String]
readUntil p = do
   line <- getLine
   if(line==p)then(return[])else(do
                                   lines <- readUntil p
                                   return (line:lines))



data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty deriving (Eq, Show)

-- Definir una función que calcule la altura de un árbol dado. Es decir la máxima distancia de la raíz a las hojas.
-- height :: BinTree a -> Int

height :: BinTree a -> Int
height (Leaf n) = 1
height (Empty) = 0
height (Node n b1 b2) = [n] ++ (preorder b1) ++ (preorder b2)
