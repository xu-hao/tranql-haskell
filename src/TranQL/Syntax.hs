module TranQL.Syntax where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Function ((&))
import Data.List (foldl')

newtype V = V String

newtype Field = Field String

data Expr = IntegerConst Integer
          | StringConst String
          | FloatConst Double
          | Var V
          | App Expr Expr
          | Let V Expr Expr
          | Fresh V T Expr
          | Select [Selector] Expr
          | Dot Expr Field

data Selector = Selector Expr Field

data T = TInteger
       | TString
       | TFloat
       | TProp
       | TRecord [TField]
       | TSet T
       | TRel T
       | TFun T T

data TField = TField Field T

-- type TEnv = [ (V, T) ]

lexer = makeTokenParser emptyDef {
    identLetter = alphaNum <|> char '_',
    reservedNames = ["select", "SELECT", "where", "WHERE", "in", "IN", "let", "LET", "fresh", "FRESH", "set", "SET", "integer", "INTEGER", "string", "STRING", "float", "FLOAT", "prop", "PROP", "set", "SET", "rel", "REL"]
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

selector :: Parser Selector
selector = Selector <$> expr <*> (ras *> field)

factor :: Parser Expr
factor = (parens lexer expr)
   <|> (IntegerConst <$> integer lexer)
   <|> (StringConst <$> stringLiteral lexer)
   <|> (FloatConst <$> float lexer)
   <|> (Var <$> var)
   <|> (Let <$> (rlet *> var <* reservedOp lexer "=") <*> (expr <* rin) <*> expr)
   <|> (Fresh <$> (rfresh *> var <* colon lexer) <*> (typep <* rin) <*> expr) 
   <|> (Select <$> (rselect *> commaSep lexer selector <* rwhere) <*> expr)

expr :: Parser Expr
expr = do
    e <- factor 
    ds <- many (do 
        reservedOp lexer "."
        f <- field
        return (flip Dot f) <|> (do
            f <- factor
            return (flip App f)))
    return (foldl' (&) e ds)

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
    (TFun t <$> typep) <|> return t
