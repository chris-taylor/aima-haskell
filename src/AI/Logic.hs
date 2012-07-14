module AI.Logic where

import Control.Applicative ((<$>))

import qualified Data.List as L

-- |A knowledge base to which you can tell and ask sentences. To create a KB,
--  create instances of this class and implement ask, tell and retract.
class KB k s q where
    -- |Add a sentence to the KB.
    tell :: k s -> s -> k s

    -- |Return a substitution that makes the query true or Nothing.
    ask :: k s -> q -> Maybe s

    -- |Retract a sentence from the KB.
    retract :: k s -> s -> k s


-- |A symbolic expression. We use this type to represent logical expressions,
--  and for terms within logical expressions.
data Expr = Val Bool
          | Var String
          | Not Expr
          | And [Expr]
          | Or [Expr]
          | Implies Expr Expr
          | Equiv Expr Expr

instance Show Expr where
    show (Val True)    = "T"
    show (Val False)   = "F"
    show (Var p)       = p
    show (Not p)       = "¬" ++ show p
    show (And ps)      = "(" ++ (concat $ L.intersperse " & " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " | " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " ⇒ " ++ show q ++ ")"
    show (Equiv p q)   = "(" ++ show p ++ " ⇔ " ++ show q ++ ")"

-- |Return a list of all the variable names in a logical expression.
vars :: Expr -> [String]
vars = L.nub . findVars
    where
        findVars (Val _) = []
        findVars (Var x) = [x]
        findVars (Not p) = findVars p
        findVars (And ps) = concatMap findVars ps
        findVars (Or ps) = concatMap findVars ps
        findVars (Implies p q) = findVars p ++ findVars q
        findVars (Equiv p q) = findVars p ++ findVars q

implies :: Bool -> Bool -> Bool
implies p q = not p || q

equiv :: Bool -> Bool -> Bool
equiv p q = implies p q && implies q p

xor :: Bool -> Bool -> Bool
xor p q = (p && not q) || (q && not p)

allBoolCombinations :: Int -> [[Bool]]
allBoolCombinations 0 = [[]]
allBoolCombinations n = do
    x  <- [True, False]
    xs <- allBoolCombinations (n-1)
    return (x:xs)

allDomains :: [String] -> [ [(String,Bool)] ]
allDomains vars = map (zip vars) (allBoolCombinations (length vars))

evaluate :: [(String,Bool)] -> Expr -> Maybe Bool
evaluate env (Val b)  = Just b
evaluate env (Var p)  = lookup p env
evaluate env (Not p)  = not <$> (evaluate env p)
evaluate env (And ps) = and <$> (mapM (evaluate env) ps)
evaluate env (Or ps)  = or  <$> (mapM (evaluate env) ps)
evaluate env (Implies p q) = do
    x <- evaluate env p
    y <- evaluate env q
    return (implies x y)
evaluate env (Equiv p q) = do
    x <- evaluate env p
    y <- evaluate env q
    return (equiv x y)

evaluateInAllModels :: Expr -> [Bool]
evaluateInAllModels expr = map eval $ allDomains (vars expr)
    where
        eval env = case evaluate env expr of
            Nothing -> error "Should never see this."
            Just v  -> v

isTautology :: Expr -> Bool
isTautology expr = and $ evaluateInAllModels expr

isContradiction :: Expr -> Bool
isContradiction expr = and $ map not $ evaluateInAllModels expr

-----------------------------
-- Conjunctive Normal Form --
-----------------------------

-- |Convert a propositional logical sentence to conjunctive normal form.
toCnf :: Expr -> Expr
toCnf = distributeAndOverOr . moveNotInward . eliminateImpl

-- |Convert implication and equality into and, or and not.
eliminateImpl :: Expr -> Expr
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
moveNotInward :: Expr -> Expr
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
distributeAndOverOr :: Expr -> Expr
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

-----------
-- Utils --
-----------

-- |Reduce an expression to associative form, i.e. flatten out all nested lists
--  of And and Or.
associate :: Expr -> Expr
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

