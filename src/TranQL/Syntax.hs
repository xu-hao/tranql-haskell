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
          | Return Expr
          | Select [Selector] Expr
          | From V Expr Expr
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

keywords :: [String]
keywords = ["select", "where", "in", "let", "fresh", "set", "integer", "string", "float", "prop", "set", "rel", "assume", "as", "return", "from"]

lexer = T.makeTokenParser emptyDef {
    T.identLetter = alphaNum <|> char '_',
    T.reservedNames = keywords ++ map (map toUpper) keywords,
    T.reservedOpNames = ["=", ":", ".", "->"]
}

reserved :: String -> Parser ()
reserved s = T.reserved lexer s <|> T.reserved lexer (map toUpper s)

reservedOp :: String -> Parser ()
reservedOp s = T.reservedOp lexer s

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

identifier :: Parser String
identifier = T.identifier lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

integer :: Parser Integer
integer = T.integer lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

float :: Parser Double
float = T.float lexer

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer

var :: Parser V
var = V <$> identifier

field :: Parser Field
field = Field <$> identifier

string :: Parser String
string = lexeme (char '\'' *> manyTill anyChar (char '\''))

rlet :: Parser ()
rlet = reserved "let"

rselect :: Parser ()
rselect = reserved "select"

rwhere :: Parser ()
rwhere = reserved "where"

rin :: Parser ()
rin = reserved "in"

rfresh :: Parser ()
rfresh = reserved "fresh"

rfloat :: Parser ()
rfloat = reserved "float"

rinteger :: Parser ()
rinteger = reserved "integer"

rstring :: Parser ()
rstring = reserved "string"

rprop :: Parser ()
rprop = reserved "prop"

rset :: Parser ()
rset = reserved "set"

rrel :: Parser ()
rrel = reserved "rel"

ras :: Parser ()
ras = reserved "as"

rreturn :: Parser ()
rreturn = reserved "return"

rlambda :: Parser ()
rlambda = reserved "assume"

rfrom :: Parser ()
rfrom = reserved "from"

selector :: Parser Selector
selector = Selector <$> expr <*> (ras *> field)

factor :: Parser Expr
factor = parens expr
   <|> (IntegerConst <$> integer )
   <|> (StringConst <$> stringLiteral )
   <|> (FloatConst <$> float )
   <|> (Var <$> var)
   <|> (do
    rlambda
    vts <- commaSep ((,) <$> (var <* reservedOp ":") <*> typep)
    rin
    e <- expr
    return (foldr (uncurry Abs) e vts))
   <|> (do
    rlet
    ves <- commaSep (do
        v <- var
        (do
            reservedOp "="
            e <- expr
            return (Let v e)) <|>
            (do
                rfrom
                e <- expr
                return (From v e)))
    rin
    e <- expr
    return (foldr ($) e ves))
   <|> (do
    rfresh
    vts <- commaSep ((,) <$> (var <* reservedOp ":") <*> typep)
    rin
    e <- expr
    return (foldr (uncurry Fresh) e vts)) 
    <|> (Return <$> (rreturn *> expr))
    <|> (Select <$> (rselect *> commaSep selector <* rwhere) <*> expr)

     
expr :: Parser Expr
expr = do
    e <- factor 
    ds <- many (reservedOp "." *> (flip Dot <$> field))
    as <- many (flip App <$> factor)
    return (foldl' (&) (foldl' (&) e ds) as)

tfactor :: Parser T
tfactor = parens typep
      <|> (rinteger $> TInteger)
      <|> (rstring $> TString)
      <|> (rfloat $> TFloat)
      <|> (rprop $> TProp)
      <|> (TRecord <$> braces  (commaSep  tfield))
      <|> (rset *> (TSet <$> typep))
      <|> (rrel *> (TRel <$> typep))

tfield :: Parser TField
tfield = TField <$> field <*> (reservedOp ":" *> typep)

typep :: Parser T
typep = do
    t <- tfactor
    (TFun t <$> (reservedOp "->" *> typep)) <|> return t

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""    