module Overlap where
import Data.List

overlap :: Int -> Int -> Int -> Int -> Bool
overlap a b c d = let [(x1, y1), (x2, y2)] = sort([(a, b), (c, d)]) in y1 >= x2 && y1 <= y2