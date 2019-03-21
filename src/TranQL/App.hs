module TranQL.App where

import TranQL.Syntax
import TranQL.Compiler
import TranQL.Exec
import TranQL.QueryParsers

interpret :: String -> IO (Either String String)
interpret s = 
    case parseWithEof expr queryParsers s of
        Left err -> return (Left (show err))
        Right expr -> 
            let s2 = compile expr in do
                putStrLn ("Executing: " ++ s2)
                res <- exec s2
                case res of
                    Left err2 -> return (Left err2)
                    Right s3 -> return (Right s3)
