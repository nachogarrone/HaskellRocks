module Clamp where

clamp :: Int -> Int -> Int -> Int
clamp a b x = if x < a then a else if x > b then b else x

