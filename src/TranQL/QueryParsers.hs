module TranQL.QueryParsers where

import Data.Map.Strict
import TranQL.Syntax
import TranQL.Query.ICEES.Syntax

queryParsers :: QueryParserMap
queryParsers = fromList [
    ("ICEES", iceesQueryParser)
    ]