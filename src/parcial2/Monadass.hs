module Monadass where

readN :: Integer -> IO [String]
readN 0 = return []
readN n
    | n > 0  = do
        x <- getLine
        res <- (readN (n-1))
        return (x:res)
    | otherwise = error "n no puede ser un número negativo"

