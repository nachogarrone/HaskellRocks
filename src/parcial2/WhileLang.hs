module WhileLang where

data AExp = Num Double | Var String | Add AExp AExp | Mult AExp AExp | Sub AExp AExp  | Div AExp AExp deriving (Show)

data BExp = ConsTrue | ConsFalse | ConsEq AExp AExp | ConsLT AExp AExp | ConsGT AExp AExp | ConsNot BExp | ConsAnd BExp BExp | ConsOr BExp BExp deriving (Show)

data Stmt = Assign String AExp | Seq [Stmt] | IfThenElse BExp Stmt Stmt | WhileDo BExp Stmt deriving (Show)

type State = (String -> Maybe Double)
---------------------------------------------------------------------------------------------------


emptyState = (\_ -> Nothing)

evalAExp :: AExp -> State -> Double
evalAExp (Num n) state = n
evalAExp (Var s) state =  state s
evalAExp (Add a b) state = (evalAExp a state) + (evalAExp b state)
evalAExp (Mult a b) state = (evalAExp a state) * (evalAExp b state)
evalAExp (Sub a b) state = (evalAExp a state) - (evalAExp b state)
evalAExp (Div a b) state = (evalAExp a state) / (evalAExp b state)
