{-# LANGUAGE FlexibleContexts #-}

module AI.Logic.Core where

import Control.Monad.Error
import Control.Monad.State

import AI.Util.Util

----------------
-- Data Types --
----------------

type ThrowsError   = Either LogicError
data LogicError    = ParseError
                   | InvalidExpression
                   | UnknownCommand
                   | DefaultError deriving (Show)

instance Error LogicError where noMsg = DefaultError

-----------------
-- Expressions --
-----------------

-- |Class for logical expressions, supporting only a single method 'parseExpr'
--  which parses a 'String' into an expression.
class (Show p, Eq p) => Expr p where
    -- |Parse a 'String' as a logical expression.
    parseExpr :: String -> ThrowsError p

--------------------
-- Knowledge Base --
--------------------

-- |A class for knowledge bases, supporting operations 'tell' (to tell a new
--  fact), 'ask' (to query the knowledge base) and 'retract' (to un-tell a
--  fact). You should create instances of this specific to your application.
class Expr p => KB k p where
    -- |Returns an empty knowledge base.
    empty :: k p

    -- |Store a new fact in the knowledge base.
    tell :: k p-> p -> k p

    -- |Query the knowledge base.
    ask :: k p -> p -> Bool

    -- |Retract a fact from the knowledge base.
    retract :: k p -> p -> k p

    -- |List the propositions stored in the knowledge base.
    axioms :: k p -> [p]

    -- |Ask if the knowledge base contains a particular fact.
    contains :: k p -> p -> Bool
    contains kb p = p `elem` axioms kb

