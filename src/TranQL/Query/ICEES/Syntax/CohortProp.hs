module TranQL.Query.ICEES.Syntax.CohortProp where

import Text.Parsec (many)
import Data.List (foldl')
import TranQL.Syntax (TranQLParser, Expr(..))
import TranQL.Query.ICEES.Syntax.Common

factor :: TranQLParser Expr
factor = do
    loperand <- iceesIdentifier
    opc <- iceesOp
    roperand <- iceesExpr
    return (Const "ICond" `App` loperand `App` opc `App` roperand)
        
propParser :: TranQLParser Expr
propParser = do
    symbol "year"
    reservedOp "="
    year <- iceesExpr
    as <- many (rand *> factor)
    return (Const "YearCohortProp" `App` year `App` case as of
        [] -> Const "ITrue"
        a : as -> foldl' (\a b -> Const "IAnd" `App` a `App` b) a as)

