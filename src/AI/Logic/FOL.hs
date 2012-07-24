module AI.Logic.FOL where

import Control.Monad.Error
import Data.Map (Map, (!))
import Data.Unique
import System.IO.Unsafe
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr

import qualified Data.List as L
import qualified Data.Map as M

import AI.Logic.Core
import AI.Util.Util

----------------
-- Data Types --
----------------

data Term = Var String
          | Sym String
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
    show (Sym x)     = x
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

conjuncts :: FOLExpr -> [FOLExpr]
conjuncts (And ps) = ps
conjuncts e        = [e]

isPred :: FOLExpr -> Bool
isPred (Pred _ _) = True
isPred _          = False

termVars :: Term -> [String]
termVars (Var x) = [x]
termVars (Sym _) = [ ]
termVars (Func _ args) = L.nub (concatMap termVars args)

-----------------
-- Unification --
-----------------

data UnificationExpr = UVar String
                     | UTerm Term
                     | UExpr FOLExpr
                     | UList [UnificationExpr]
                     deriving (Eq)

-- |Unify expressions @x@, @y@ with subsitution @theta@. Return either a
--  substitution that would make @x@, @y@ equal, or 'Nothing' if @x@ and @y@
--  cannot unify.
unify :: FOLExpr -> FOLExpr -> Maybe (Map String Term)
unify x y = unify' (UExpr x) (UExpr y) (Just M.empty)

unify' :: UnificationExpr -> UnificationExpr -> Maybe (Map String Term) -> Maybe (Map String Term)
unify' x y  Nothing = Nothing
unify' x y (Just theta)
    | x == y  = Just theta
    | isVar x = unifyVar (getVar x) y theta
    | isVar y = unifyVar (getVar y) x theta
    | isExpr x && isExpr y =
        unify' (getArgs x) (getArgs y) (unify' (getOp x) (getOp y) (Just theta))
    | isList x && isList y && (getLength x == getLength y) = 
        unify' (getRest x) (getRest y) (unify' (getHd x) (getHd y) (Just theta))
    | otherwise = Nothing

unifyVar :: String -> UnificationExpr -> Map String Term -> Maybe (Map String Term)
unifyVar var x theta
    | M.member var theta = unify' (UTerm $ theta ! var) x (Just theta)
    | isVar x && M.member (getVar x) theta =
        unify' (UVar var) (UTerm $ theta ! getVar x) (Just theta)
    | occurCheck var x   = Nothing
    | otherwise          = Just (M.insert var (getTerm x) theta)

-- |Return 'True' if @var@ occurs anywhere in @x@.
occurCheck :: String -> UnificationExpr -> Bool
occurCheck var x
    | isVar x   = getVar x == var
    | isExpr x  = occurCheck var (getArgs x)
    | isList x  = any (occurCheck var) (getList x)
    | otherwise = False

----------------------
-- Definite Clauses --
----------------------

data Statement = Statement { symbol :: String
                           , args :: [Term] } deriving (Eq)

data DefiniteClause = DC { premises :: [Statement]
                         , conclusion :: Statement } deriving (Eq)

instance Show Statement where
    show (Statement sym []  ) = sym
    show (Statement sym args) = sym ++ "(" ++ commaSep (map show args) ++  ")"

instance Show DefiniteClause where
    show (DC []    conc) = show conc
    show (DC prems conc) =
        concat (L.intersperse " & " $ map show prems) ++ " => " ++ show conc

isFact :: DefiniteClause -> Bool
isFact = null . premises

toFact :: Statement -> DefiniteClause
toFact s = DC [] s

facts :: [DefiniteClause] -> [DefiniteClause]
facts = filter isFact

toStatement :: FOLExpr -> Statement
toStatement (Pred sym args) = Statement sym args

toDefiniteClause :: FOLExpr -> ThrowsError DefiniteClause
toDefiniteClause (Pred sym args) = return $ DC [] (Statement sym args)
toDefiniteClause (Implies p q)   = if all isPred xs && isPred q
    then return $ DC (map toStatement xs) (toStatement q)
    else throwError InvalidExpression
    where
        xs = conjuncts p

toExpr :: DefiniteClause -> FOLExpr
toExpr (DC [] c) = statementToExpr c
toExpr (DC ps c) = And (map statementToExpr ps) `Implies` statementToExpr c

statementToExpr :: Statement -> FOLExpr
statementToExpr s = Pred (symbol s) (args s)

clauseVars :: DefiniteClause -> [String]
clauseVars (DC prems conc) =
    L.nub $ concatMap statementVars prems ++ statementVars conc

statementVars :: Statement -> [String]
statementVars (Statement _ args) = L.nub $ concatMap termVars args

subst :: DefiniteClause -> Map String Term -> DefiniteClause
subst (DC ps c) m = DC (map renameS ps) (renameS c)
    where
        renameS (Statement sym args) = Statement sym (map renameT args)

        renameT (Var x) = m!x
        renameT (Sym x) = Sym x
        renameT (Func x args) = Func x (map renameT args)

stUnify :: DefiniteClause -> DefiniteClause -> Maybe (Map String Term)
stUnify s1 s2 = unify (toExpr s1) (toExpr s2)

----------------------
-- Forward Chaining --
----------------------

fc :: [DefiniteClause] -> Statement -> Maybe (Map String Term)
fc kb a = unsafePerformIO (run kb)
    where
        run (r:rs) = do
            DC ps q <- standardizeApart r

            let subs          = getMatchingSubs ps (L.delete r kb)
                (result, new) = apply q [] Nothing subs

            if no result
                then if null new
                        then return Nothing
                        else run (kb ++ new)
                else return result 
        
        apply _ new (Just phi) _      = (Just phi, new)         
        apply _ new _          []     = (Nothing, new)
        apply q new _          (t:ts) = if isRenaming q (kb ++ new)
            then apply q new Nothing ts
            else apply q (q':new) (stUnify q' $ toFact a) ts
            where q' = subst (toFact q) t

isRenaming :: Statement -> [DefiniteClause] -> Bool
isRenaming s kb = undefined

getMatchingSubs :: [Statement] -> [DefiniteClause] -> [Map String Term]
getMatchingSubs ps kb = undefined

----------------------
-- Rename Variables --
----------------------

standardizeApart :: DefiniteClause -> IO DefiniteClause
standardizeApart dc = uniqify (clauseVars dc) >>= return . subst dc

uniqify :: [String] -> IO (Map String Term)
uniqify = foldM func M.empty
    where func m s = do s' <- mkUnique s
                        return $ M.insert s (Var s') m

mkUnique :: String -> IO String
mkUnique s = newUnique >>= \n -> return (':' : s ++ show (hashUnique n))

----------------------------------
-- Unification Helper Functions --
----------------------------------

isVar :: UnificationExpr -> Bool
isVar (UVar _) = True
isVar _        = False

getVar :: UnificationExpr -> String
getVar (UVar x) = x
getVar _        = error "Not a variable -- AI.Logic.FOL.getVar"

isTerm :: UnificationExpr -> Bool
isTerm (UTerm _) = True
isTerm _         = False

getTerm :: UnificationExpr -> Term
getTerm (UTerm t) = t
getTerm _         = error "Not a term -- AI.Logic.FOL.getTerm"

isExpr :: UnificationExpr -> Bool
isExpr (UExpr _) = True
isExpr _         = False

getArgs :: UnificationExpr -> UnificationExpr
getArgs (UExpr x) = UList (go x)
    where go (Not p)       = map UExpr [p]
          go (And ps)      = map UExpr ps
          go (Or ps)       = map UExpr ps
          go (Implies p q) = map UExpr [p,q]
          go (Pred p ts)   = map toU ts
            where toU (Var x) = UVar x
                  toU term    = UTerm term
getArgs _ = error "Not an expression -- AI.Logic.FOL.getArgs"

getOp :: UnificationExpr -> UnificationExpr
getOp (UExpr x) = UTerm $ Sym (go x)
    where go (Not p)       = "Not"
          go (And ps)      = "And"
          go (Or ps)       = "Or"
          go (Implies p q) = "Implies"
          go (Pred p ts)   = p
getOp _ = error "Not an expression -- AI.Logic.FOL.getOp"

isList :: UnificationExpr -> Bool
isList (UList _) = True
isList _         = False

getList :: UnificationExpr -> [UnificationExpr]
getList (UList xs) = xs
getList _          = error "Not a list -- AI.Logic.FOL.getList"

getLength :: UnificationExpr -> Int
getLength (UList xs) = length xs
getLength _          = error "Not a list -- AI.Logic.FOL.getLength"

getHd :: UnificationExpr -> UnificationExpr
getHd (UList xs) = head xs
getHd _          = error "Not a list -- AI.Logic.FOL.getHd"

getRest :: UnificationExpr -> UnificationExpr
getRest (UList xs) = UList (tail xs)
getRest _          = error "Not a list -- AI.Logic.FOL.getRest"

------------------------------
-- First-Order Logic Parser --
------------------------------

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

parseTerm :: Parser Term
parseTerm = try parseFunc
        <|> parseVar
        <|> parseSym
        <?> "Variable, Symbol or Function"

parseVar :: Parser Term
parseVar = do
    x <- name
    return $ Var x

parseSym :: Parser Term
parseSym = do
    x <- capsName
    return $ Sym x

parseFunc :: Parser Term
parseFunc = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Func x ts

parseVal :: Parser FOLExpr
parseVal = (char 'T' >> return (Val True))
       <|> (char 'F' >> return (Val False))
       <?> "T or F"

parsePred :: Parser FOLExpr
parsePred = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Pred x ts

parseForAll :: Parser FOLExpr
parseForAll = do
    string "forall" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ ForAll x e

parseExists :: Parser FOLExpr
parseExists = do
    string "exists" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ Exists x e

capsName :: Parser String
capsName = do
    c  <- upper
    cs <- many alphaNum
    return (c:cs)

name :: Parser String
name = do
    c  <- lower
    cs <- many alphaNum
    return (c:cs)

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
    , [binary "=>" Implies AssocRight] ]

binary name fun assoc = Infix (do{ string name; return fun }) assoc
prefix name fun       = Prefix (do{ string name; return fun })

