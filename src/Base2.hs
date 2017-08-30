module Base2 where

base2 :: Int -> String
base2 a = if a < 0 then
  error "Negative numbers don't have a base 2 representation"
  else if
    a == 0 then "0" else
      if a == 1 then "1" else
        (base2 (div a 2)) ++ (show (mod a 2))