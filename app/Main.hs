module Main where

import TranQL.App
import System.Environment
import Options.Applicative
import Control.Monad.Trans.Except (runExcept)

data Config = Config {
    query :: String
}

config :: Parser Config
config = Config
    <$> argument str (metavar "QUERY")


main :: IO ()
main = do
    let opts = info config (fullDesc <> progDesc "Parse and type check TranQL expressions" <> header "A program for parsing and type checking TranQL expressions")
    options <- execParser opts
    s <- interpret (query options)
    case s of
        Left s -> putStrLn ("Failure: " ++ s)
        Right s -> putStrLn ("Success: " ++ s)
    