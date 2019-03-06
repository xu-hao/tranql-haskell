module TranQL.Syntax where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Function ((&))
import Data.List (foldl')

newtype V = V String deriving (Eq, Show)

newtype Field = Field String deriving (Eq, Show)

data Expr = IntegerConst Integer
          | StringConst String
          | FloatConst Double
          | Var V
          | App Expr Expr
          | Abs V T Expr
          | Let V Expr Expr
          | Fresh V T Expr
          | Select [Selector] Expr
          | Dot Expr Field deriving (Eq, Show)

data Selector = Selector Expr Field deriving (Eq, Show)

data T = TInteger
       | TString
       | TFloat
       | TProp
       | TRecord [TField]
       | TSet T
       | TRel T
       | TFun T T deriving (Eq, Show)

data TField = TField Field T deriving (Eq, Show)

lexer = makeTokenParser emptyDef {
    identLetter = alphaNum <|> char '_',
    reservedNames = ["select", "SELECT", "where", "WHERE", "in", "IN", "let", "LET", "fresh", "FRESH", "set", "SET", "integer", "INTEGER", "string", "STRING", "float", "FLOAT", "prop", "PROP", "set", "SET", "rel", "REL", "assume", "ASSUME"],
    reservedOpNames = ["=", ":", ".", "->"]
}

var :: Parser V
var = V <$> identifier lexer

field :: Parser Field
field = Field <$> identifier lexer

string :: Parser String
string = lexeme lexer (char '\'' *> manyTill anyChar (char '\''))

rlet :: Parser ()
rlet = reserved lexer "let" <|> reserved lexer "LET"

rselect :: Parser ()
rselect = reserved lexer "select" <|> reserved lexer "SELECT"

rwhere :: Parser ()
rwhere = reserved lexer "where" <|> reserved lexer "WHERE"

rin :: Parser ()
rin = reserved lexer "in" <|> reserved lexer "IN"

rfresh :: Parser ()
rfresh = reserved lexer "fresh" <|> reserved lexer "FRESH"

rfloat :: Parser ()
rfloat = reserved lexer "float" <|> reserved lexer "FLOAT"

rinteger :: Parser ()
rinteger = reserved lexer "integer" <|> reserved lexer "INTEGER"

rstring :: Parser ()
rstring = reserved lexer "string" <|> reserved lexer "STRING"

rprop :: Parser ()
rprop = reserved lexer "prop" <|> reserved lexer "PROP"

rset :: Parser ()
rset = reserved lexer "set" <|> reserved lexer "SET"

rrel :: Parser ()
rrel = reserved lexer "rel" <|> reserved lexer "REL"

ras :: Parser ()
ras = reserved lexer "as" <|> reserved lexer "AS"

rlambda :: Parser ()
rlambda = reserved lexer "assume" <|> reserved lexer "ASSUME"

selector :: Parser Selector
selector = Selector <$> expr <*> (ras *> field)

factor :: Parser Expr
factor = parens lexer expr
   <|> (IntegerConst <$> integer lexer)
   <|> (StringConst <$> stringLiteral lexer)
   <|> (FloatConst <$> float lexer)
   <|> (Var <$> var)
   <|> (Abs <$> (rlambda *> var <* reservedOp lexer ":") <*> (typep <* rin) <*> expr)
   <|> (Let <$> (rlet *> var <* reservedOp lexer "=") <*> (expr <* rin) <*> expr)
   <|> (Fresh <$> (rfresh *> var <* reservedOp lexer ":") <*> (typep <* rin) <*> expr) 
   <|> (Select <$> (rselect *> commaSep lexer selector <* rwhere) <*> expr)

expr :: Parser Expr
expr = do
    e <- factor 
    ds <- many (reservedOp lexer "." *> (flip Dot <$> field))
    as <- many (flip App <$> factor)
    return (foldl' (&) (foldl' (&) e ds) as)

tfactor :: Parser T
tfactor = (parens lexer typep)
      <|> (rinteger *> pure TInteger)
      <|> (rstring *> pure TString)
      <|> (rfloat *> pure TFloat)
      <|> (rprop *> pure TProp)
      <|> (TRecord <$> braces lexer (commaSep lexer tfield))
      <|> (rset *> (TSet <$> typep))
      <|> (rrel *> (TRel <$> typep))

tfield :: Parser TField
tfield = TField <$> field <*> (reservedOp lexer ":" *> typep)

typep :: Parser T
typep = do
    t <- tfactor
    (TFun t <$> (reservedOp lexer "->" *> typep)) <|> return t

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""    