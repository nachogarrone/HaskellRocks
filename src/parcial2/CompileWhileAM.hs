module CompileWhileAM where

import Data.Either
import Data.Maybe

data AExp = Num Int | Var String | Add AExp AExp | Mult AExp AExp | Sub AExp AExp deriving (Show)
data BExp = ConsTrue | ConsFalse | CompEq AExp AExp | CompLtEq AExp AExp | LogNot BExp | LogAnd BExp BExp deriving (Show)
data Stmt = Assign String AExp | Seq [Stmt] | IfThenElse BExp Stmt Stmt | WhileDo BExp Stmt deriving (Show)

data Inst = Push Int | ADD | MULT | SUB
    | CompEq2 | CompLe2 | LogAnd2 | LogNot2 | ConstTrue2 | ConstFalse2
    | Fetch String | Store String | Noop
    | Branch [Inst] [Inst] | Loop [Inst] [Inst]
    deriving (Show)

type State = (String -> Maybe Int)
type Stack = [Either Int Bool]
type StackState = (Stack, State)

emptyState = (\_ -> Nothing)

compileAExp (Num n) = [Push n]
compileAExp (Var s) = [Fetch s]

compileBExp (ConsTrue) = [ConstTrue2]
compileBExp (ConsFalse) = [ConstFalse2]
