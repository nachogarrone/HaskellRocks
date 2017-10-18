module Monadass where

readN :: Integer -> IO [String]
readN 0 = return []
readN n = do
    x <- getLine
    res <- (readN (n-1))
    return (x:res)

