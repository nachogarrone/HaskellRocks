module Ejercicio2 where

roundP :: Double -> Double -> Double
roundP n p = let d = 10 ** p
    in fromInteger (round (n * d)) / d
