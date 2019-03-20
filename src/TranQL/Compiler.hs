module TranQL.Compiler where

import TranQL.Syntax
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

compile :: Expr -> String
compile (IntegerConst i) = show i
compile (FloatConst d) = show d
compile (StringConst s) = show s
compile (Var (V x)) = x
compile (App e1 e2) = 
        let e1c = compile e1
            e2c = compile e2 in
            "(" ++ e1c ++ " " ++ e2c ++ ")"
compile (Abs (V x) e) =
        let ec = compile e in
            "(\\" ++ x ++ " -> " ++ ec ++ ")"
compile (Return e) =
        let ec = compile e in
            "(return " ++ ec ++ ")"
compile (Bind e1 e2) =
        let e1c = compile e1
            e2c = compile e2 in
            "(" ++ e1c ++ " >>= " ++ e2c ++ ")"

        