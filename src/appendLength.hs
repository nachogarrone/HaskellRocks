--Torterolo, Caamaño, Fernández
appendLength::[[Int]]->[[Int]]
appendLength xs = [l:x | x<-xs] where l = length(xs)
--appendLength xs = map (y:) xs where y = length xs 
--appendLength xs = map (\x->(y):x) xs where y = length xs
--appendLength xs = map (\x->(length xs):x) xs