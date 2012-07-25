{-# LANGUAGE FlexibleContexts #-}

-- |This module defines types and type classes that are used in the other Logic
--  modules. 
module AI.Logic.Core where

import Control.Monad.Error
import Control.Monad.State
import Data.Map (Map)

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
--  fact). Instances of 'KB' are used in routines in the @Logic.Interative@
--  package.
class (Expr p, Show t) => KB k p t where
    -- |Returns an empty knowledge base.
    empty :: k p t

    -- |Store a new fact in the knowledge base.
    tell :: k p t -> p -> k p t

    -- |Ask whether a particular statement is entailed by the knowledge base.
    ask :: k p t -> p -> Bool

    -- |Given a statement containing variables, return an assignment of
    --  variables to terms that satisfies the statement.
    askVars :: k p t -> p -> [Map String t]

    -- |Retract a fact from the knowledge base.
    retract :: k p t -> p -> k p t

    -- |List the propositions stored in the knowledge base.
    axioms :: k p t -> [p]

    -- |Ask if the knowledge base contains a particular fact.
    contains :: k p t -> p -> Bool
    contains kb p = p `elem` axioms kb

