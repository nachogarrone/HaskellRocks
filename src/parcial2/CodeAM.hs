module CodeAM where

data Inst = Push Int | ADD | MULT | SUB
    | CompEq | CompLe | LogAnd | LogNot | ConstTrue | ConstFalse
    | Fetch String | Store String | Noop
    | Branch [Inst] [Inst] | Loop [Inst] [Inst]
    deriving (Show)

type State = (String -> Maybe Int)
