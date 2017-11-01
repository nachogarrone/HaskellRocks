module Lambda where

data Lambda = Var Int
    | Apl Lambda Lambda
    | Abs Int Lambda deriving(Eq, Show)