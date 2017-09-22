parseBits::String->[Int]
parseBits xs = map getBit2 xs


getBit::Char->Int
getBit c 
    | c == '1' = 1
    | c == '0' = 0
    | otherwise = error "no es un bit!"


getBit2::Char->Int
getBit2 c 
    | (==) c '1' = 1
    | (==) c '0' = 0
    | otherwise = error "no es un bit!"