module AI.Logic.Parser (parsePL) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token hiding (parens)

import AI.Logic.Propositional

--------------------------------
-- Propositional Logic Parser --
--------------------------------

-- |Parse a 'String' as an expression in propositional logic.
parsePL :: String -> Maybe Expr
parsePL input = case parse expr "" input of
    Left _  -> Nothing
    Right x -> return x

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

parseVal :: Parser Expr
parseVal = (char 'T' >> return (Val True))
       <|> (char 'F' >> return (Val False))
       <?> "T or F"

parseVar :: Parser Expr
parseVar = do
    c  <- letter
    cs <- many alphaNum
    return $ Var (c:cs)

term :: Parser Expr
term = parens expr
   <|> parseVal
   <|> parseVar
   <?> "T, F or proposition name"

parens :: Parser a -> Parser a
parens p = do
    char '(' >> spaces
    x <- p
    spaces >> char ')'
    return x

table = 
    [ [prefix "~" Not]
    , [binary "&" (\x y -> And [x,y]) AssocLeft]
    , [binary "|" (\x y -> Or [x,y]) AssocLeft]
    , [binary "=>" Implies AssocRight]
    , [binary "<=>" Equiv AssocRight] ]

binary name fun assoc = Infix (do{ string name; return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })
