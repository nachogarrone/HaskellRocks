parseBits::String->[Int]
parseBits xs = map (\x-> if(x=='0') then 0 else 1)) xs
