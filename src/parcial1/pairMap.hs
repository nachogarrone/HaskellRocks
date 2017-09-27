--Caamaño, Torterolo, Fernández
pairMap::(a->b)->((a,a)->(b,b))
pairMap f (x, y) = (f x, f y)
--pairMap f = (\(x,y) -> (f x, f y))