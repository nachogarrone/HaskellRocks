module Algebraicos where


data Month = Ene | Feb | Mar | Abr | May | Jun | Jul | Ago | Sep | Oct | Nov | Dic deriving (Eq, Show)

data Step = StepLeft | StepRight | StepUp | StepDown deriving (Eq, Show)

-- monthDays:: Month -> Int -> Int
monthDays Feb y = if (esBisiesto y) then 29 else 28
monthDays Ene _ = 31
monthDays Mar _ = 31
monthDays Abr _ = 30
monthDays May _ = 31
monthDays Jun _ = 30
monthDays Jul _ = 31
monthDays Ago _ = 31
monthDays Sep _ = 30
monthDays Oct _ = 31
monthDays Nov _ = 30
monthDays Dic _ = 31

esBisiesto::Int->Bool
esBisiesto x = ( mod x 400==0) || (mod x 4==0) && not (mod x 100==0)

walk :: (Int, Int) -> [Step] -> (Int, Int)
walk a [] = a
walk (a,b) (x:xs)
    | x == StepLeft = walk (a-1, b) xs
    | x == StepRight = walk (a+1, b) xs
    | x == StepUp = walk (a, b+1) xs
    | x == StepDown = walk (a, b-1) xs
