module Algebraicos where


data Month = Ene | Feb | Mar | Abr | May | Jun | Jul | Ago | Sep | Oct | Nov | Dic deriving (Eq, Show)

-- data Step = StepLeft | StepRight | StepUp | StepDown deriving (Eq, Show)
data Step2 = StepLeft Int | StepRight Int | StepUp Int | StepDown Int deriving (Eq, Show)

data UKLength = Yards Double | Feet Double | Inches Double deriving (Eq, Show)


data RPS = Piedra | Papel | Tijera deriving (Eq, Show)

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

-- walk :: (Int, Int) -> [Step] -> (Int, Int)
-- walk a [] = a
-- walk (a,b) (x:xs)
--     | x == StepLeft = walk (a-1, b) xs
--     | x == StepRight = walk (a+1, b) xs
--     | x == StepUp = walk (a, b+1) xs
--     | x == StepDown = walk (a, b-1) xs

walk2 a [] = a
walk2 (a,b) ((StepLeft n):xs) = walk2 (a-n, b) xs
walk2 (a,b) ((StepRight n):xs) = walk2 (a+n, b) xs
walk2 (a,b) ((StepUp n):xs) = walk2 (a, b+n) xs
walk2 (a,b) ((StepDown n):xs) = walk2 (a, b-n) xs
-- walk2 (a,b) ((x n):xs)
--     | x == StepLeft = walk2 (a-n, b) xs
--     | x == StepRight = walk2 (a+n, b) xs
--     | x == StepUp = walk2 (a, b+n) xs
--     | x == StepDown = walk2 (a, b-n) xs

-- gameRPS2 (ax,az) (bx,bz) = compare (compareRPS ax az) (compareRPS bx bz)
gameRPS a b = (compareRPS a b)

compareRPS Piedra Tijera = 1
compareRPS Piedra Papel = -1
compareRPS Piedra Piedra = 0
compareRPS Tijera Piedra = -1
compareRPS Tijera Papel = 1
compareRPS Tijera Tijera = 0
compareRPS Papel Tijera = -1
compareRPS Papel Piedra = 1
compareRPS Papel Papel = 0


toInches (Feet n) = (Inches (n * 12))
toInches (Inches n) = (Inches n)
toInches (Yards n) = (Inches (n*36))

toFeet (Feet n) = (Feet n)
toFeet (Inches n) = (Feet (n/12))
toFeet (Yards n) = (Feet (n*3))

toYards (Feet n) = (Yards (n/3))
toYards (Inches n) = (Yards (n*0.0277778))
toYards (Yards n) = (Yards n)
