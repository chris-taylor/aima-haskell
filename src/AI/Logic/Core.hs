{-# LANGUAGE FlexibleContexts #-}

module AI.Logic.Core where

import Control.Monad.Error
import Control.Monad.State

import AI.Util.Util

----------------
-- Data Types --
----------------

data LogicError    = ParseError | UnknownCommand | DefaultError deriving (Show)
type ThrowsError   = Either LogicError
type IOThrowsError = ErrorT LogicError IO
type Logic k       = StateT k IOThrowsError

instance Error LogicError where noMsg = DefaultError

-- |Run a computation of type `Logic k'. The computation represents a live
--  interaction with a knowledge base. We don't care about the result - we
--  just want to get the side effects from storing premises in the knowledge
--  base and querying it for new information.
runLogic :: (Error e, Monad m) => StateT s (ErrorT e m) a -> s -> m ()
runLogic c s = ignoreResult $ runErrorT $ evalStateT c s

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

