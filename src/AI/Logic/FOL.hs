module AI.Logic.FOL where

import qualified Data.List as L

data Term = Var String
          | Func String [Term]

data Expr = Val Bool
          | Pred String [Term]
          | Not Expr
          | And [Expr]
          | Or  [Expr]
          | Implies Expr Expr
          | ForAll String Expr
          | Exists String Expr

instance Show Term where
    show (Var x)     = x
    show (Func f xs) = f ++ "(" ++ commaSep (map show xs) ++ ")"

instance Show Expr where
    show (Val True)    = "T"
    show (Val False)   = "F"
    show (Not p)       = "¬" ++ show p
    show (And ps)      = "(" ++ (concat $ L.intersperse " ∧ " $ map show ps) ++ ")"
    show (Or ps)       = "(" ++ (concat $ L.intersperse " ∨ " $ map show ps) ++ ")"
    show (Implies p q) = "(" ++ show p ++ " ⇒ " ++ show q ++ ")"
    show (ForAll x p)  = "∀" ++ x ++ ". " ++ show p
    show (Exists x p)  = "∃" ++ x ++ ". " ++ show p
    show (Pred p xs)   = p ++ "(" ++ commaSep (map show xs) ++  ")"

commaSep :: [String] -> String
commaSep xs = concat $ L.intersperse "," xs

