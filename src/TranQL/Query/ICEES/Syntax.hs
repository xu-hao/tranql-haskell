module TranQL.Query.ICEES.Syntax where

import TranQL.Syntax
import qualified TranQL.Query.ICEES.Syntax.Selector as S


iceesQueryParser :: QueryParser
iceesQueryParser = QueryParser {
    selectorParser = S.selectorParser
    }

