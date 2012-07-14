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
    show (And ps)      = "(" ++ (concat $ L.intersperse " ∧ " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " ∨ " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " ⇒ " ++ show q ++ ")"
    show (Equiv p q)   = "(" ++ show p ++ " ⇔ " ++ show q ++ ")"

-- |The expression that is always true.
true :: Expr
true = Val True

-- |The expression that is always false.
false :: Expr
false = Val False

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

-- |Does the first logical expression entail the second? This algorithm uses
--  truth tables.
ttEntails :: Expr -> Expr -> Bool
ttEntails s t = and $ ttCheck (s `Implies` t)

-- |Helper function for ttEntails. Evaluates the expression in all possible
--  models.
ttCheck :: Expr -> [Bool]
ttCheck expr = map check $ allModels (vars expr)
    where   
        check model = case plTrue model expr of
            Nothing -> error "Should never see this."
            Just v  -> v

        allModels vars = map (zip vars) (enumerateBool (length vars))

        enumerateBool 0 = [[]]
        enumerateBool n = do
            x  <- [True, False]
            xs <- enumerateBool (n-1)
            return (x:xs)

-- |Is the propositional sentence a tautology - is it true in all possible
--  models (i.e. is it entailed by true?)
ttTrue :: Expr -> Bool
ttTrue s = true `ttEntails` s

-- |Is the propositional sentence a contradiction - is it false in all
--  possible models (i.e. does it entail false?)
ttFalse :: Expr -> Bool
ttFalse s = s `ttEntails` false

-- |Return 'True' if the propositional logic expression is true in the model,
--  and 'False' if it is false. If the model does not specify the value for
--  every proposition then return 'Nothing' (note that this may happen even
--  when the expression is tautological).
plTrue :: [(String,Bool)] -> Expr -> Maybe Bool
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

