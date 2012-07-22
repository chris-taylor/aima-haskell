module AI.Logic.FOL where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
--import qualified Text.Parsec.Token

import qualified Data.List as L

import AI.Logic.Core
import AI.Util.Util

----------------
-- Data Types --
----------------

data Term = Var String
          | Func String [Term]
          deriving (Eq)

data FOLExpr = Val Bool
             | Pred String [Term]
             | Not FOLExpr
             | And [FOLExpr]
             | Or  [FOLExpr]
             | Implies FOLExpr FOLExpr
             | ForAll String FOLExpr
             | Exists String FOLExpr
             deriving (Eq)

instance Show Term where
    show (Var x)     = x
    show (Func f xs) = f ++ "(" ++ commaSep (map show xs) ++ ")"

instance Show FOLExpr where
    show (Val True)    = "T"
    show (Val False)   = "F"
    show (Not p)       = "~" ++ show p
    show (And ps)      = "(" ++ (concat $ L.intersperse " & " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " | " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
    show (ForAll x p)  = "forall " ++ x ++ ". " ++ show p
    show (Exists x p)  = "exists " ++ x ++ ". " ++ show p
    show (Pred p xs)   = p ++ "(" ++ commaSep (map show xs) ++  ")"

instance Expr FOLExpr where
    parseExpr str = case parseFOL str of
        Nothing -> throwError ParseError
        Just e  -> return e

------------------------------
-- First Order Logic Parser --
------------------------------

-- |Parse a 'String' as an expression in propositional logic.
parseFOL :: String -> Maybe FOLExpr
parseFOL input = case parse expr "" input of
    Left _  -> Nothing
    Right x -> return x

expr :: Parser FOLExpr
expr = buildExpressionParser table term <?> "expression"

term :: Parser FOLExpr
term = parens expr
   <|> try parseVal
   <|> try parsePred
   <|> parseForAll
   <|> parseExists
   <?> "T, F or expression"


-- |Parse a first-order logic term - either a variable or a function.
parseTerm :: Parser Term
parseTerm = parseVar <|> parseFunc <?> "Variable or Function"

-- |Parse a variable in first-order logic.
parseVar :: Parser Term
parseVar = do
    x <- name
    return $ Var x

-- |Parse a function in first-order logic.
parseFunc :: Parser Term
parseFunc = do
    x  <- name
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Func x ts

-- |Parse a boolean value in first-order logic.
parseVal :: Parser FOLExpr
parseVal = (char 'T' >> return (Val True))
       <|> (char 'F' >> return (Val False))
       <?> "T or F"

-- |Parse a predicate in first-order logic.
parsePred :: Parser FOLExpr
parsePred = do
    x  <- name
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Pred x ts

-- |Parse a forall statement in first-order logic.
parseForAll :: Parser FOLExpr
parseForAll = do
    string "forall" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ ForAll x e

-- |Parse an /exists/ statement in first-order logic.
parseExists :: Parser FOLExpr
parseExists = do
    string "exists" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ Exists x e


table = 
    [ [prefix "~" Not]
    , [binary "&" (\x y -> And [x,y]) AssocLeft]
    , [binary "|" (\x y -> Or [x,y]) AssocLeft]
    , [binary "=>" Implies AssocRight] ]

binary name fun assoc = Infix (do{ string name; return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })


name :: Parser String
name = do
    c  <- letter
    cs <- many alphaNum
    return (c:cs)

parens :: Parser a -> Parser a
parens p = do
    char '(' >> spaces
    x <- p
    spaces >> char ')'
    return x

