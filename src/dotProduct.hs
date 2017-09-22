--Torterolo, Caamaño, Fernández
dotProduct::[Int]->[Int]->Int
dotProduct x y  
    | x == [] && y ==[] = error "dotProduct:Los vectores están vacíos"
    | length(x)/= length(y) = error "dotProduct:Los vectores son de diferente largo"
	| otherwise = sum (zipWith (*) x y)
