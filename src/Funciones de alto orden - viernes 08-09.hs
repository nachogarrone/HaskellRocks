import Data.Char
import Data.List

parseBits :: String -> [Int]
parseBits texto = map filtrarBits texto

filtrarBits :: Char -> Int
filtrarBits c = if elem c "01" then digitToInt(c) else error "error"

factorial :: Int -> Int
factorial a = if(a>0)then foldl (*) 1 [1..a] else error "error"

parseBin :: String -> Int
parseBin t = foldl (\x y -> x*2+y) 0 (parseBits t)

pairMap :: (a->b)->((a,a)->(b,b))
pairMap f (a,b) = (f a, f b)

pairMap2 f = (\(a,b)->(f a, f b))

map2 :: (a->a->b)->[a]->[b]
map2 f l = zipWith f l (drop 1 l)