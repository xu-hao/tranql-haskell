module TranQL.Query.ICEES.Syntax.AssociationsToAllFeaturesProp where

import TranQL.Syntax (TranQLParser, Expr(..))
import TranQL.Query.ICEES.Semantics
import TranQL.Query.ICEES.Syntax.Common

propParser :: TranQLParser Expr
propParser = do
    symbol "year"
    reservedOp "="
    year <- iceesExpr
    rand
    symbol "cohort_id"
    reservedOp "="
    cohort_id <- iceesExpr
    rand
    loperand <- iceesIdentifier
    op <- iceesOp
    roperand <- iceesExpr
    rand
    symbol "p_value"
    reservedOp "<"
    maximum_p_value <- iceesExpr
    return (Const "ATAFProp" `App` year `App` cohort_id `App` loperand `App` op `App` roperand `App` maximum_p_value)

