module CodeAM where

import Data.Either
import Data.Maybe

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

evalAM (ConstTrue:xs) (stack, state) = evalAM xs ((Right True):stack, state)
evalAM (ConstFalse:xs) (stack, state) = evalAM xs ((Right False):stack, state)
evalAM (CompEq:m) ((Left x):(Left y):xs, state) = evalAM m ((Right (x==y)):xs,state)
evalAM (CompLe:m) ((Left x):(Left y):xs, state) = evalAM m ((Right (x<=y)):xs,state)
evalAM (LogNot:m) ((Right x):xs, state) = evalAM m ((Right (not x)):xs,state)
evalAM (LogAnd:m) ((Right x):(Right y):xs, state) = evalAM m ((Right (x&&y)):xs,state)

evalAM ((Push n):xs) (stack, state) = evalAM xs ((Left n):stack, state)
evalAM ((Fetch s):xs) (stack, state) = evalAM xs ((Left (fromJust(state s))):stack, state)
evalAM ((Store s):xs) ((st:stack), state) = evalAM xs (stack, state2)
    where state2 = (\v -> if v == s then Just st else state s)
