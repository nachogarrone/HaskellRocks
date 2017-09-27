module HexDigit where
import Data.List

-- TIENE ERRORES
hexDigit :: Char -> Int
hexDigit h = if elem h ['0', '1', '2', '3', '4', '5', '6', '7','8', '9'] then h
    else if elem (h toLower) ["a,b,c,d,e,f"] then h
      else error "-1"

