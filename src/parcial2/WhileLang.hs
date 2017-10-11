module WhileLang where

data AExp = Num Double | Var String | Add AExp AExp | Mult AExp AExp | Sub AExp AExp deriving (Show)

data BExp = ConsTrue | ConsFalse | CompEq AExp AExp | CompLtEq AExp AExp | LogNot BExp | LogAnd BExp BExp deriving (Show)

data Stmt = Assign String AExp | Seq [Stmt] | IfThenElse BExp Stmt Stmt | WhileDo BExp Stmt deriving (Show)

type State = String -> Maybe Double
---------------------------------------------------------------------------------------------------


emptyState = (\_ -> Nothing)

evalAExp :: AExp -> State -> Double
evalAExp (Num n) state = n
evalAExp (Var s) state =  state s
evalAExp (Add a b) state = (evalAExp a state) + (evalAExp b state)
evalAExp (Mult a b) state = (evalAExp a state) * (evalAExp b state)
evalAExp (Sub a b) state = (evalAExp a state) - (evalAExp b state)

evalBExp :: BExp -> State -> Bool
evalBExp ConsTrue _ = True
evalBExp ConsFalse _ = False
evalBExp (CompEq a1 a2) s = (evalAExp a1 s) == (evalAExp a2 s)
evalBExp (CompLtEq a1 a2) s = (evalAExp a1 s) <= (evalAExp a2 s)
evalBExp (LogNot b1) s = not (evalBExp b1 s)
evalBExp (LogAnd b1 b2) s = (evalBExp b1 s) && (evalBExp b2 s)

evalStmt :: Stmt -> State -> State
evalStmt (Assign x a) s = (\v -> if v == x then (evalAExp a s) else (s v))
evalStmt (Seq is) s = foldl ff s is where ff i s = evalStmt i s
evalStmt (IfThenElse b s1 s2) s = if (evalBExp b s) then (evalStmt s1 s) else (evalStmt s2 s)
evalStmt w@(WhileDo b i) s = if (evalBExp b s) then (evalStmt w (evalStmt i s)) else s
