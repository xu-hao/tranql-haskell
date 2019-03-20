module TranQL.Syntax where

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
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity

newtype V = V String deriving (Eq, Show)

data Expr = IntegerConst Integer
          | StringConst String
          | FloatConst Double
          | Const String
          | Var V
          | App Expr Expr
          | Abs V Expr
          | Return Expr
          | Bind Expr Expr
          | Select String Expr Expr deriving (Eq, Show)

newtype QueryParser = QueryParser {
    selectorParser :: TranQLParser (Expr, QueryPropParser)
}

data QueryPropParser = QueryPropParser {
    propParser :: TranQLParser Expr,
    trueProp :: Maybe Expr
}
          
type QueryParserMap = Map String QueryParser
type TranQLParser = ParsecT String () (Reader QueryParserMap)



keywords :: [String]
keywords = ["select", "from", "where", "let", "in", "assume", "return"]

langDef :: T.GenLanguageDef String () (Reader QueryParserMap)
langDef = T.LanguageDef {
    T.reservedNames = keywords ++ map (map toUpper) keywords,
    T.reservedOpNames = ["=", "<-"],
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

reserved :: String -> TranQLParser ()
reserved s = T.reserved lexer s <|> T.reserved lexer (map toUpper s)

reservedOp :: String -> TranQLParser ()
reservedOp = T.reservedOp lexer

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

lexeme :: TranQLParser a -> TranQLParser a
lexeme = T.lexeme lexer

var :: TranQLParser V
var = V <$> identifier

string :: TranQLParser String
string = lexeme (char '\'' *> manyTill anyChar (char '\''))

rlet :: TranQLParser ()
rlet = reserved "let"

rselect :: TranQLParser ()
rselect = reserved "select"

rwhere :: TranQLParser ()
rwhere = reserved "where"

rin :: TranQLParser ()
rin = reserved "in"

rreturn :: TranQLParser ()
rreturn = reserved "return"

rlambda :: TranQLParser ()
rlambda = reserved "assume"

rfrom :: TranQLParser ()
rfrom = reserved "from"

factor :: TranQLParser Expr
factor = parens expr
   <|> (IntegerConst <$> integer )
   <|> (StringConst <$> stringLiteral )
   <|> (FloatConst <$> float )
   <|> (Var <$> var)
   <|> (do
    rlambda
    vs <- commaSep var
    rin
    e <- expr 
    return (foldr Abs e vs))
   <|> (do
    rlet
    ves <- commaSep (do
        v <- var
        (do
            reservedOp "="
            e <- expr
            return (\e2 -> App (Abs v e2) e)) <|>
            (do
                reservedOp "<-"
                e <- expr
                return (Bind e . Abs v)))
    rin
    e <- expr
    return (foldr ($) e ves))
   <|> (Return <$> (rreturn *> expr))
   <|> (do
        rfrom
        source <- identifier
        queryParsers <- lift ask
        case M.lookup source queryParsers of
            Just (QueryParser selectorP) -> do
                rselect
                (selectorExpr, QueryPropParser propP tProp) <- selectorP
                propExpr <- case tProp of
                    Just tProp -> (rwhere *> propP) <|> (pure tProp)
                    Nothing -> rwhere *> propP
                return (Const "select" `App` Const ("@" ++ source) `App` selectorExpr `App` propExpr)
            Nothing -> 
                fail ("cannot find query source " ++ source))

     
expr :: TranQLParser Expr
expr = do
    e <- factor
    as <- many (flip App <$> factor)
    return (foldl' (&) e as)

parseWithEof :: TranQLParser a -> QueryParserMap -> String -> Either ParseError a
parseWithEof p queryParsers s = runReader (runParserT (p <* eof) () "" s) queryParsers