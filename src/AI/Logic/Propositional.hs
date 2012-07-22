{-# LANGUAGE TypeSynonymInstances #-}

module AI.Logic.Propositional where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.State
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token hiding (parens)

import AI.Logic.Core
import qualified Data.List as L

import AI.Util.Util

----------------
-- Data Types --
----------------

-- |A symbolic expression. We use this type to represent logical expressions,
--  and for terms within logical expressions.
data PLExpr = Val Bool
            | Var String
            | Not PLExpr
            | And [PLExpr]
            | Or  [PLExpr]
            | Implies PLExpr PLExpr
            | Equiv PLExpr PLExpr
            deriving (Eq)

instance Show PLExpr where
    show (Val True)    = "T"
    show (Val False)   = "F"
    show (Var p)       = p
    show (Not p)       = "~" ++ show p
    show (And ps)      = "(" ++ (concat $ L.intersperse " & " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " | " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
    show (Equiv p q)   = "(" ++ show p ++ " <=> " ++ show q ++ ")"

-- |Expressions in propositional logic can be parsed using 'parsePL'.
instance Expr PLExpr where
    parseExpr str = case parsePL str of
        Nothing -> throwError ParseError
        Just e  -> return e

---------------------
-- Knowledge Bases --
---------------------

-- |A simple knowledge base for propositional logic. We keep a list of known
--  propositions (the axioms) to be used as an inference base.
data PropKB p = PropKB [PLExpr]

-- |An instance of 'KB' for propositional knowledge bases. It uses the
--  'plResolution' algorithm to determine if a query is entailed by the KB.
instance KB PropKB PLExpr where
    empty                  = PropKB []
    tell     (PropKB ps) p = PropKB $ ps ++ conjuncts (toCnf p)
    retract  (PropKB ps) p = PropKB $ L.delete p ps
    ask      (PropKB ps) q = plResolution (And ps) q
    axioms   (PropKB ps)   = ps

-- |Concrete instance of a propositional logic knowledge base that will use
--  truth tables for inference.
data PropTTKB p = PropTTKB [PLExpr]

-- |The 'KB' instance for a knowledge base that uses truth tables for inference.
instance KB PropTTKB PLExpr where
    empty                    = PropTTKB []
    tell     (PropTTKB ps) p = PropTTKB $ ps ++ conjuncts (toCnf p)
    retract  (PropTTKB ps) p = PropTTKB $ L.delete p ps
    ask      (PropTTKB ps) q = ttEntails (And ps) q
    axioms   (PropTTKB ps)   = ps

-----------------------------------
-- Propositional Logic Utilities --
-----------------------------------

-- |The expression that is always true.
true :: PLExpr
true = Val True

-- |The expression that is always false.
false :: PLExpr
false = Val False

-- |Return 'True' if an expression is an atom (i.e. if it is top, bottom, or a
--  symbol).
isAtom :: PLExpr -> Bool
isAtom (Val _) = True
isAtom (Var _) = True
isAtom _       = False

-- |Return a list of all conjuncts in a logical expression.
conjuncts :: PLExpr -> [PLExpr]
conjuncts (And xs) = xs
conjuncts other    = [other]

-- |Return a list of all disjuncts in a logical expression.
disjuncts :: PLExpr -> [PLExpr]
disjuncts (Or xs) = xs
disjuncts other   = [other]

-- |Return a list of all the variable names in a logical expression.
vars :: PLExpr -> [String]
vars = L.nub . findVars
    where
        findVars (Val _) = []
        findVars (Var x) = [x]
        findVars (Not p) = findVars p
        findVars (And ps) = concatMap findVars ps
        findVars (Or ps) = concatMap findVars ps
        findVars (Implies p q) = findVars p ++ findVars q
        findVars (Equiv p q) = findVars p ++ findVars q

----------------------------
-- Truth Table Entailment --
----------------------------

-- |Does the first logical expression entail the second? This algorithm uses
--  truth tables (Fig 7.10).
ttEntails :: PLExpr -> PLExpr -> Bool
ttEntails s t = and $ ttCheck (s `Implies` t)

-- |Helper function for ttEntails. Evaluates the expression in all possible
--  models.
ttCheck :: PLExpr -> [Bool]
ttCheck expr = map check $ allModels (vars expr)
    where   
        check model = case plTrue model expr of
            Nothing -> error "Should never see this."
            Just v  -> v
            
        allModels vars = map (zip vars) (bools $ length vars)

-- |Is the propositional sentence a tautology - is it true in all possible
--  models (i.e. is it entailed by true?)
ttTrue :: PLExpr -> Bool
ttTrue s = true `ttEntails` s

-- |Is the propositional sentence a contradiction - is it false in all
--  possible models (i.e. does it entail false?)
ttFalse :: PLExpr -> Bool
ttFalse s = s `ttEntails` false

-- |Return 'True' if the propositional logic expression is true in the model,
--  and 'False' if it is false. If the model does not specify the value for
--  every proposition then return 'Nothing' (note that this may happen even
--  when the expression is tautological).
plTrue :: [(String,Bool)] -> PLExpr -> Maybe Bool
plTrue model (Val b)  = Just b
plTrue model (Var p)  = lookup p model
plTrue model (Not p)  = not <$> (plTrue model p)
plTrue model (And ps) = and <$> (mapM (plTrue model) ps)
plTrue model (Or ps)  = or  <$> (mapM (plTrue model) ps)
plTrue model (Implies p q) = do
    x <- plTrue model p
    y <- plTrue model q
    return (not x || y)
plTrue model (Equiv p q) = do
    x <- plTrue model p
    y <- plTrue model q
    return (x == y)

-----------------------------
-- Conjunctive Normal Form --
-----------------------------

-- |Convert a propositional logical sentence to conjunctive normal form.
toCnf :: PLExpr -> PLExpr
toCnf = associate . distributeAndOverOr . moveNotInward . eliminateImpl

-- |Convert implication and equality into and, or and not.
eliminateImpl :: PLExpr -> PLExpr
eliminateImpl (p `Implies` q) = Or [Not p', q']
    where p' = eliminateImpl p
          q' = eliminateImpl q
eliminateImpl (p `Equiv` q) = And [ Or [Not p', q'], Or [Not q', p'] ]
    where p' = eliminateImpl p
          q' = eliminateImpl q
eliminateImpl (Not p)  = Not (eliminateImpl p)
eliminateImpl (And ps) = And (map eliminateImpl ps)
eliminateImpl (Or ps)  = Or  (map eliminateImpl ps)
eliminateImpl other    = other

-- |Given a sentence which is a sequence of conjunctions, disjunctions and
--  negations, rewrite a sentence by moving all the negation operators inward.
moveNotInward :: PLExpr -> PLExpr
moveNotInward (Not s)  = case s of
    And ps -> Or  $ map (moveNotInward . Not) ps
    Or ps  -> And $ map (moveNotInward . Not) ps
    Not s     -> s
    expr      -> Not expr
moveNotInward (And ps) = And $ map moveNotInward ps
moveNotInward (Or  ps) = Or  $ map moveNotInward ps
moveNotInward expr     = expr

-- |Given a sequence of conjunctions and disjunctions, put it into conjunctive
--  normal form by distributing all of the disjunctions across the conjunctions.
distributeAndOverOr :: PLExpr -> PLExpr
distributeAndOverOr (Or ps) = foldr1 distribute (map distributeAndOverOr ps)
    where
        distribute (And xs) p = And $ map distributeAndOverOr [ Or [x,p] | x <- xs ]
        distribute p (And xs) = And $ map distributeAndOverOr [ Or [p,x] | x <- xs ]
        distribute (Or xs) p  = Or (xs ++ [p])
        distribute p (Or xs)  = Or (p:xs)
        distribute p q        = Or [p,q]
distributeAndOverOr (And ps) = And (map distributeAndOverOr ps)
distributeAndOverOr (Not p)  = Not (distributeAndOverOr p)
distributeAndOverOr expr     = expr

-- |Reduce an expression to associative form, i.e. flatten out all nested lists
--  of And and Or.
associate :: PLExpr -> PLExpr
associate (And ps) = And $ foldr f [] (map associate ps)
    where f (And xs) ys = xs ++ ys
          f expr ys     = expr : ys
associate (Or ps)  = Or  $ foldr f [] (map associate ps)
    where f (Or xs) ys = xs ++ ys
          f expr ys    = expr : ys
associate (Not p) = Not (associate p)
associate (p `Implies` q) = associate p `Implies` associate q
associate (p `Equiv` q)   = associate p `Equiv` associate q
associate expr     = expr

----------------
-- Resolution --
----------------

-- |Return 'True' if s and t are complementary literals.
complementary :: PLExpr -> PLExpr -> Bool
complementary (Not p) q = isAtom p && isAtom q && p == q
complementary p (Not q) = isAtom p && isAtom q && p == q
complementary _ _       = False

-- |Return 'True' if a disjunction of literals can be demonstrated to be a
--  tautology without need of evaluation - this is the case if it contains two
--  complementary literals.
isTautology :: PLExpr -> Bool
isTautology expr = or $ map (uncurry complementary) clausePairs
    where
        clausePairs = unorderedPairs (disjuncts expr)

-- |A resolution algorithm for propositional logic. It performs a mechanical
--  form of proof by contradiction. It first builds the set of clauses of the
--  /negation/ of the statement to be demonstrated, and then resolves every
--  pair of clauses. If a contradiction is generated, then the algorithm returns
--  'True', otherwise it continues. If at any stage, no new clauses can be
--  generated, then the algorithm returns 'False'.
plResolution :: PLExpr -> PLExpr -> Bool
plResolution s t = go $ conjuncts $ toCnf $ And [s, Not t]
    where
        go clauses = if contradictionDerived
            then True
            else if new `isSubSet` clauses
                then False
                else go (L.union clauses new)

            where
                (contradictionDerived, new) =
                    foldr resolve (False, []) (unorderedPairs clauses)

        resolve (_,_) (True, new)  = (True, new)
        resolve (x,y) (False, new) = if false `elem` resolvents
            then (True, new)
            else (False, L.union new resolvents)
            where
                resolvents = plResolve x y

-- |Return the set of all possible clauses obtained by resolving the two inputs.
plResolve :: PLExpr -> PLExpr -> [PLExpr]
plResolve p q =
    filter (not . isTautology) [resolve x y | x <- ps, y <- qs, complementary x y]
    where
        ps = disjuncts p
        qs = disjuncts q

        resolve x y = case L.union (L.delete x ps) (L.delete y qs) of
            [] -> Val False
            xs -> Or xs

----------------------
-- Definite Clauses --
----------------------

-- |Return 'True' if an expression in propositional logic is a literal, i.e. if
--  it is a symbol, or the negation of a symbol.
isLiteral :: PLExpr -> Bool
isLiteral (Not p) = isAtom p
isLiteral p       = isAtom p

-- |Return 'True' if a literal expression in propositional logic is positive,
--  i.e. it is not a negation.
isPositive :: PLExpr -> Bool
isPositive (Not p) = False
isPositive _       = True

-- |Return 'True' if an expression in propositional logic is a definite clause,
--  i.e. if it is a disjunction of literals, exactly one of which is positive.
isDefiniteClause :: PLExpr -> Bool
isDefiniteClause expr = all isLiteral xs && countIf isPositive xs == 1
    where xs = disjuncts expr

-- |Return 'True' if an expresssion in propositional logic is a Horn clause,
--  i.e. if it is at disjunction of literals, at most one of which is positive.
isHornClause :: PLExpr -> Bool
isHornClause expr = all isLiteral xs && countIf isPositive xs < 2
    where xs = disjuncts expr

--------------------------------
-- Propositional Logic Parser --
--------------------------------

-- |Parse a 'String' as an expression in propositional logic.
parsePL :: String -> Maybe PLExpr
parsePL input = case parse expr "" input of
    Left _  -> Nothing
    Right x -> return x

expr :: Parser PLExpr
expr = buildExpressionParser table term <?> "expression"

parseVal :: Parser PLExpr
parseVal = (char 'T' >> return (Val True))
       <|> (char 'F' >> return (Val False))
       <?> "T or F"

parseVar :: Parser PLExpr
parseVar = do
    c  <- letter
    cs <- many alphaNum
    return $ Var (c:cs)

term :: Parser PLExpr
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
