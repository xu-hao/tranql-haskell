module Main where

import TranQL.Syntax
import System.Environment
import Options.Applicative
import TranQL.Syntax (expr, parseWithEof)
import TranQL.Semantics.Static (TEnv, typeCheck)
import Control.Monad.Trans.Except (runExcept)

data Config = Config {
    query :: String,
    typecheck :: Bool
}

config :: TranQLParser Config
config = Config
    <$> argument str (metavar "QUERY")
    <*> switch (long "typecheck")

builtIn :: TEnv
builtIn = [
    (V "true", TProp),
    (V "TRUE", TProp),
    (V "false", TProp),
    (V "FALSE", TProp),
    (V "and", TFun TProp (TFun TProp TProp)),
    (V "AND", TFun TProp (TFun TProp TProp)),
    (V "or", TFun TProp (TFun TProp TProp)),
    (V "OR", TFun TProp (TFun TProp TProp)),
    (V "not", TFun TProp TProp),
    (V "NOT", TFun TProp TProp)]

main :: IO ()
main = do
    let opts = info config (fullDesc <> progDesc "Parse and type check TranQL expressions" <> header "A program for parsing and type checking TranQL expressions")
    options <- execParser opts
    case parseWithEof expr (query options) of
        Left err -> print err
        Right e -> do
            print e
            if typecheck options
                then case runExcept (typeCheck builtIn e) of
                        Left err -> putStrLn err
                        Right t -> print t
                else return ()