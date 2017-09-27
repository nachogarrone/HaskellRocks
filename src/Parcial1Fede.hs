module Parcial1Fede  where

import Data.Either

f n = foldl (*) 1 [1..n]

mult x y = x * y

multA x y = return (x * y)

multB (x,y) = x * y

multD = \x y -> x * y

makeMask :: String -> [Bool]
makeMask "" = []
makeMask (x:xs)
    | x == '|' = True:makeMask xs
    | x == '.' = False:makeMask xs
    | otherwise = error "Wrong symbol!"

addI_D :: (Either Int Double) -> (Either Int Double) -> (Either Int Double)
addI_D (Left x) (Left y) = Left (x + y)
addI_D (Left x) (Right y) = Right (fromIntegral x + y)
addI_D (Right x) (Left y) = Right (x + fromIntegral y)
addI_D (Right x) (Right y) = Right (x + y)

--splitMult :: Int -> [(Int, Int)]
--splitMult a = filter (mod a x) xs
--    where (x:xs) = [1..a]