module TranQL.Exec where

import Data.List
import Control.Monad.Trans.Class (lift)

import Control.Monad
import Language.Haskell.Interpreter
    
exec :: String -> IO (Either String String)
exec stmt = do 
    r <- runInterpreter (run stmt)
    case r of
        Left err -> return (Left (errorString err))
        Right s -> return (Right s)
    
errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
      where
        header = "ERROR: Won't compile:"
        unbox (GhcError e) = e
errorString e = show e
    
-- observe that Interpreter () is an alias for InterpreterT IO ()
run :: String -> Interpreter String
run expr1 = do
    -- loadModules ["Main.hs"]
    -- setTopLevelModules ["Main"]
    set [languageExtensions := [PolyKinds]]
    setImports ["TranQL.Semantics", "TranQL.Query.ICEES.Semantics", "Prelude"]
    t <- typeOf expr1
    lift (putStrLn ("Type: " ++ show t))
    runStmt ("x <- " ++ expr1)
    eval "x"

