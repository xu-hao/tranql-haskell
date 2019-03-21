module TranQL.Query.ICEES.Syntax.Selector where

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
import TranQL.Syntax (TranQLParser, QueryParserMap, Expr(..), QueryPropParser(..))
import qualified TranQL.Query.ICEES.Syntax.CohortProp as C
import qualified TranQL.Query.ICEES.Syntax.AssociationsToAllFeaturesProp as ATAF

langDef :: T.GenLanguageDef String () (Reader QueryParserMap)
langDef = T.LanguageDef {
    T.reservedNames = [],
    T.reservedOpNames = [],
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

identifier :: TranQLParser String
identifier = T.identifier lexer

symbol :: String -> TranQLParser String
symbol = T.symbol lexer

selectorParser :: TranQLParser (Expr, QueryPropParser)
selectorParser = 
    (do
        s <- symbol "Patient" <|> symbol "Visit"
        return (Const ("S" ++ s), QueryPropParser {
            propParser = C.propParser,
            trueProp = Nothing
            })
        ) <|> (do
        s1 <- symbol "AssociationsToAllFeatures"
        s2 <- symbol "Patient" <|> symbol "Visit"
        return (Const ("S" ++ s1 ++ s2), QueryPropParser {
            propParser = ATAF.propParser,
            trueProp = Nothing
            })
            )