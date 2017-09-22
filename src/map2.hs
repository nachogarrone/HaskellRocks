--Caamaño, Torterolo, Fernández
map2::(a->a->b)->[a]->[b]
map2 f [] = []
map2 f (x:[]) = []
map2 f (x1:x2:xs) = (f x1 x2) : (map2 f (x2:xs))