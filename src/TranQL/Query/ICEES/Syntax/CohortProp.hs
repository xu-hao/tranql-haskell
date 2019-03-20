module TranQL.Query.ICEES.Syntax.CohortProp where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.Function ((&))
import Data.List (foldl')
import Data.Char (toUpper)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import TranQL.Syntax (TranQLParser, QueryParserMap, Expr(..), reserved)
import TranQL.Query.ICEES.Semantics

keywords :: [String]
keywords = ["and", "true", "false"]

langDef :: T.GenLanguageDef String () (Reader QueryParserMap)
langDef = T.LanguageDef {
    T.reservedNames = keywords ++ map (map toUpper) keywords,
    T.reservedOpNames = ["-"],
    T.commentStart   = "{-",
    T.commentEnd     = "-}",
    T.commentLine    = "--",
    T.nestedComments = True,
    T.identStart     = letter <|> char '_',
    T.identLetter    = alphaNum <|> oneOf "_'",
    T.opStart        = T.opLetter langDef,
    T.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~",
    T.caseSensitive  = True
    }

lexer :: T.GenTokenParser String () (Reader QueryParserMap)
lexer = T.makeTokenParser langDef

reservedOp :: String -> TranQLParser ()
reservedOp = T.reservedOp lexer

operator :: TranQLParser String
operator = T.operator lexer

commaSep :: TranQLParser a -> TranQLParser [a]
commaSep = T.commaSep lexer

identifier :: TranQLParser String
identifier = T.identifier lexer

braces :: TranQLParser a -> TranQLParser a
braces = T.braces lexer

parens :: TranQLParser a -> TranQLParser a
parens = T.parens lexer

integer :: TranQLParser Integer
integer = T.integer lexer

stringLiteral :: TranQLParser String
stringLiteral = T.stringLiteral lexer

float :: TranQLParser Double
float = T.float lexer

rand :: TranQLParser ()
rand = reserved "and"

factor :: TranQLParser Expr
factor = do
    loperand <- identifier
    op <- operator
    opc <- case op of
            ">" -> return "Gt"
            "<" -> return "Lt"
            ">=" -> return "Ge"
            "<=" -> return "Le"
            "=" -> return "Eq"
            "<>" -> return "Ne"
            _ -> fail ("undefined operator " ++ op)
    roperand <- (FloatConst <$> float) 
            <|> (StringConst <$> stringLiteral) 
            <|> (reserved "true" *> pure (Const "True"))
            <|> (reserved "false" *> pure (Const "False"))
            <|> (do
                i <- integer 
                (do 
                    reservedOp "-" 
                    i' <- integer
                    return (Const ("A" ++ show i ++ "_" ++ show i'))) <|> return (IntegerConst i))
    return (Const "ICond" `App` Const ("@" ++ loperand) `App` Const opc `App` roperand)
        
trueProp :: Expr
trueProp = Const "ITrue"

propParser :: TranQLParser Expr
propParser = do
    as <- sepBy factor rand
    return (case as of
        [] -> trueProp
        a : as -> foldl' (\a b -> App (App (Const "IAnd") a) b) a as)

