module AI.Logic.Parser where

--import Text.Parsec.Expr ((<?>), (<|>))
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token hiding (parens)

import AI.Logic.Propositional

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

parens :: Parser a -> Parser a
parens p = do
    char '(' >> spaces
    x <- p
    spaces >> char ')'
    return x

run :: Parser Expr -> String -> IO (Maybe Expr)
run p input = case (parse p "" input) of
    Left err -> do
        putStr "parse error at "
        print err >> return Nothing
    Right x  -> print x >> return (Just x)

term = parens expr
   <|> parseVal
   <|> parseVar
   <?> "T, F or proposition name"

table = 
    [ [prefix "~" Not]
    , [binary "&" (\x y -> And [x,y]) AssocLeft]
    , [binary "|" (\x y -> Or [x,y]) AssocLeft]
    , [binary "=>" Implies AssocRight]
    , [binary "<=>" Equiv AssocRight] ]

binary name fun assoc = Infix (do{ string name; return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })