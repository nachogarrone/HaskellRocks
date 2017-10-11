module CompileWhileAM where

import WhileLang
import CodeAM

compileAExp (Num n) = [Push n]
compileAExp (Var s) = [Fetch s]
