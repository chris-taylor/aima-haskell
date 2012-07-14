module AI.Logic where

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
--          | Xor Expr Expr


instance Show Expr where
    show (Val True)    = "T"
    show (Val False)   = "F"
    show (Var p)       = p
    show (Not p)       = "~" ++ show p
    show (And ps)      = "(" ++ (concat $ L.intersperse " & " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " | " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
    show (Equiv p q)   = "(" ++ show p ++ " <=> " ++ show q ++ ")"
    --show (Xor p q)     = "(" ++ show p ++ " ^ " ++ show q ++ ")"

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
--eliminateImpl (p `Xor` q) = Or [ And [p', Not q'], And [q', Not p'] ]
--    where p' = eliminateImpl p
--          q' = eliminateImpl q
eliminateImpl (Not p)  = Not (eliminateImpl p)
eliminateImpl (And ps) = And (map eliminateImpl ps)
eliminateImpl (Or ps)  = Or  (map eliminateImpl ps)
eliminateImpl other    = other

-- |Rewrite a sentence by moving the negation operator inward.
moveNotInward :: Expr -> Expr
moveNotInward (Not s)  = case s of
    And ps -> Or  $ map (moveNotInward . Not) ps
    Or ps  -> And $ map (moveNotInward . Not) ps
    Not s     -> s
    expr      -> Not expr
moveNotInward (And ps) = And $ map moveNotInward ps
moveNotInward (Or  ps) = Or  $ map moveNotInward ps
moveNotInward expr     = expr

distributeAndOverOr :: Expr -> Expr
distributeAndOverOr (Or ps) = foldr1 distribute ps
    where
        distribute (And xs) p = And (map (addOr p) xs)
        distribute p (And xs) = And (map (addOr p) xs)
        distribute p q        = Or [p,q]

        addOr y x = Or [y,x]
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

