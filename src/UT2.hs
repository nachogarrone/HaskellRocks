insertAt::[a]->a->Int->[a]
insertAt l y i 
    | ((length l) == 0) && (i == 0) = [y]
    | (length l) < i = error "index out of range"
    | otherwise = (take i l)++[y]++(drop i l)

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:[]) = [[x]] ++ [[]]

sublists2 :: [a] -> [[a]]
sublists2 [] = [[]]
sublists2 (x:xs) =  [[x] ++ ys | ys <- sublists2 xs]

--sublists (x:xs) =  sublists xs ++ (map (\sub -> sub  ++ [x]) (sublists xs))

