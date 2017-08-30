module FilterParen where
import Data.List

filterParen :: String -> String
filterParen "" = ""
filterParen (x:xs) = if ('(' == x) || (')' == x) then
  x:(filterParen xs) else filterParen xs

