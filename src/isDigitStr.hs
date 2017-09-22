--Torterolo, Caamaño, Fernández
import Data.Char
isDigitStr::String->Bool
isDigitStr s =  null (dropWhile (isDigit) s)
