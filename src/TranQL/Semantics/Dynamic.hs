module TranQL.Semantics.Dynamic where

import TranQL.Syntax
import Data.Map.Strict (Map)

data Value = IntegerValue Integer
           | StringValue String
           | FloatValue Double
           | RecordValue (Map String Value)
           | SetValue [Value]

type Env = [ (V, Value) ]