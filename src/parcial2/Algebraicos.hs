module Algebraicos where


data Month = Ene | Feb | Mar | Abr | May | Jun | Jul | Ago | Sep | Oct | Nov | Dic deriving (Eq, Show)

data Step = StepLeft | StepLeft | StepLeft | StepLeft deriving (Eq, Show)

-- monthDays:: Month -> Int -> Int
monthDays x y
    | Ene y = 31
    | Feb y = if (esBisiesto y) then 29 else 28
    | Mar y = 31
    | Abr y = 30
    | May y = 31
    | Jun y = 30
    | Jul y = 31
    | Ago y = 31
    | Sep y = 30
    | Oct y = 31
    | Nov y = 30
    | Dic y = 31

esBisiesto::Int->Bool
esBisiesto x = ( mod x 400==0) || (mod x 4==0) && not (mod x 100==0)
