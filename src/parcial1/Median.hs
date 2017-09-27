module Median where
import Data.List
import Prelude

median :: Int -> Int -> Int-> Int
median a b c = let o = sort([a, b, c]) in o !! 1
