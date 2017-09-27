module Algebraicos where


data Month = Ene | Feb | Mar | Abr | May | Jun | Jul | Ago | Sep | Oct | Nov | Dic deriving (Eq, Show)

-- data Step = StepLeft | StepLeft | StepLeft | StepLeft deriving (Eq, Show)

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
