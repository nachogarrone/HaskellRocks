parseBin::String->Int
parseBin xs =foldl (+) 0 (zipWith (\x y -> x*(2^y)) (map getBit xs) (reverse [0..(length xs)-1]))


parseBin2 xs =foldl (+) 0 (zipWith (*) (map getBit xs) (map (2^)(reverse [0..(length xs)-1])))

getBit::Char->Int
getBit c 
    | c == '1' = 1
    | c == '0' = 0
    | otherwise = error "no es un bit!" 


