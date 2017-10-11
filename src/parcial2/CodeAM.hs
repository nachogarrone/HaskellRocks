module CodeAM where

import Data.Either

data Inst = Push Int | ADD | MULT | SUB
    | CompEq | CompLe | LogAnd | LogNot | ConstTrue | ConstFalse
    | Fetch String | Store String | Noop
    | Branch [Inst] [Inst] | Loop [Inst] [Inst]
    deriving (Show)

type State = (String -> Maybe Int)
type Stack = [Either Int Bool]
type StackState = (Stack, State)

emptyState = (\_ -> Nothing)

evalAM :: [Inst] -> StackState -> StackState
evalAM [] a = a
evalAM [ConstTrue] ([], s) = ([Right True], s)
evalAM [ConstFalse] ([], s) = ([Right False], s)
