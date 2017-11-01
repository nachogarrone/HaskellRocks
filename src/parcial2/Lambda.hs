module Lambda where

import Data.List

data Lambda = Var Int
    | Apl Lambda Lambda
    | Abs Int Lambda deriving(Eq, Show)



freevars :: Lambda -> [Int]

freevars (Var x) = [x]
freevars (Apl e f) = union (freevars e) (freevars f)
freevars (Abs x e) = delete x (freevars e)


substitution :: Lambda -> Int -> Lambda -> Lambda

substitution e@(Var x) y r
    | y == x = r
    | otherwise = e

substitution (Apl e f) x r =
    (Apl (substitution e x r) (substitution f x r))

substitution e@(Abs x f) y r
    | x == y = e
    | otherwise = (Abs x (substitution f y r))


betaRedexes :: Lambda -> [Lambda]

betaRedexes e@(Apl (Abs _ m) n) =
    e:((betaRedexes m) ++ (betaRedexes n))
betaRedexes (Apl m n) =
    (betaRedexes m) ++ (betaRedexes n)
betaRedexes (Abs _ m) = betaRedexes m
betaRedexes _ = []

