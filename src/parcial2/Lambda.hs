module Lambda where

import Data.List

data Lambda = Var Int
    | Apl Lambda Lambda
    | Abs Int Lambda deriving(Eq, Show)



freevars :: Lambda -> [Int]

freevars (Var x) = [x]
freevars (Apl e f) = union (freevars e) (freevars f)
freevars (Abs x e) = delete x (freevars e)