module Entorno where
import Data.List

entorno [] = []
entorno [a] = [[a-1], [a+1]]
entorno (x:xs) = [(x-1:xs), (x+1:xs)] ++ [x:y | y <- entorno xs]
