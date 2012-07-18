{-# LANGUAGE FlexibleContexts #-}

module AI.Logic.Core where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import qualified Data.List as L

import AI.Logic.Parser
import AI.Logic.Propositional
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
class (Show p, Eq p) => LogicExpr p where
    -- |Parse a 'String' as a logical expression.
    parseExpr :: String -> ThrowsError p

-- |Expressions in propositional logic can be parsed using 'parsePL'.
instance LogicExpr Expr where
    parseExpr str = case parsePL str of
        Nothing -> throwError ParseError
        Just e  -> return e

--------------------
-- Knowledge Base --
--------------------

-- |A class for knowledge bases, supporting operations 'tell' (to tell a new
--  fact), 'ask' (to query the knowledge base) and 'retract' (to un-tell a
--  fact). You should create instances of this specific to your application.
class LogicExpr p => KB k p where
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

-- |A simple knowledge base for propositional logic. We keep a list of known
--  propositions (the axioms) to be used as an inference base.
data PropKB p = PropKB [Expr]

-- |An instance of 'KB' for propositional knowledge bases. It uses the
--  'plResolution' algorithm to determine if a query is entailed by the KB.
instance KB PropKB Expr where
    empty                  = PropKB []
    tell     (PropKB ps) p = PropKB $ ps ++ conjuncts (toCnf p)
    retract  (PropKB ps) p = PropKB $ L.delete p ps
    ask      (PropKB ps) q = plResolution (And ps) q
    axioms   (PropKB ps)   = ps

-- |Concrete instance of a propositional logic knowledge base that will use
--  truth tables for inference.
data PropTTKB p = PropTTKB [Expr]

-- |The 'KB' instance for a knowledge base that uses truth tables for inference.
instance KB PropTTKB Expr where
    empty                    = PropTTKB []
    tell     (PropTTKB ps) p = PropTTKB $ ps ++ conjuncts (toCnf p)
    retract  (PropTTKB ps) p = PropTTKB $ L.delete p ps
    ask      (PropTTKB ps) q = ttEntails (And ps) q
    axioms   (PropTTKB ps)   = ps

----------------------
-- Interactive Code --
----------------------

-- |Start an interaction with a propositional logic theorem prover that uses
--  a resolution algorithm to do inference.
runProp :: IO ()
runProp = do
    putStrLn "Propositional Logic Resolution Theorem Prover"
    runLogic loop (empty :: PropKB Expr)

-- |Start an interaction with a propositional logic theorem prover that uses
--  truth tables to do inference.
runTT :: IO ()
runTT = do
    putStrLn "Propositional Logic Truth Table Theorem Prover"
    runLogic loop (empty :: PropTTKB Expr)

-- |The input/output loop for a theorem prover. We repeatedly ask for input
--  from the user, and then dispatch on the result, until the user enters the
--  command @"quit"@.
loop :: KB k p => Logic (k p) ()
loop = untilM (== "quit") (liftIO readPrompt) (trapError . dispatch)

-- |Decide what to do with user input to a theorem prover.
dispatch :: KB k p => String -> Logic (k p) ()
dispatch str = case cmd of
    "show"      -> get >>= (\kb -> liftIO $ showPremises kb)
    "help"      -> liftIO showHelp
    "tell"      -> parse rest >>= tellKB
    "ask"       -> parse rest >>= askKB
    "retract"   -> parse rest >>= retractKB
    "clear"     -> clear
    ""          -> return ()
    _           -> liftIO unknown
    where
        (cmd,rest) = break (== ' ') str

-- |Parse an expression entered by the user to be passed into either `tellKB',
--  `askKB' or `retractKB'.
parse :: KB k p => String -> Logic (k p) p
parse str = case parseExpr (strip str) of
    Left _  -> liftIO (putStrLn "***parse error") >> throwError ParseError
    Right p -> return p

-- |Store a new premise in the knowledge base.
tellKB :: KB k p => p -> Logic (k p) ()
tellKB expr = modify (\kb -> tell kb expr)

-- |Query the knowledge base.
askKB :: KB k p => p -> Logic (k p) ()
askKB expr = do
    kb <- get
    liftIO $ showPremises kb
    if ask kb expr
        then liftIO $ putStrLn $ "Entailed: " ++ show expr
        else liftIO $ putStrLn $ "Not entailed: " ++ show expr

-- |Remove a premise from the knowledge base.
retractKB :: KB k p => p -> Logic (k p) ()
retractKB expr = do
    kb <- get
    if kb `contains` expr
        then modify $ \kb -> retract kb expr
        else liftIO $ putStrLn $ "***expression " ++ show expr ++ " not in KB"

-- |Empty the knowledge base of all previously entered premises.
clear :: KB k p => Logic (k p) ()
clear = put empty

-- |Display a list of all premises currently stored in the knowledge base.
showPremises :: KB k p => k p -> IO ()
showPremises kb = forM_ (enumerate $ axioms kb) $
    \(n,p) -> putStrLn ("  " ++ show n ++ ". " ++ show p)

-- |IO routine to deal with unrecognised commands.
unknown :: IO ()
unknown = putStrLn "***unknown command"

-- |Show a useful help message for an interaction with a theorem prover.
showHelp :: IO ()
showHelp = do
    putStrLn "  tell <p>    Store proposition <p> in the knowledge base"
    putStrLn "  retract <p> Remove proposition <p> from the knowledge base"
    putStrLn "  ask <p>     Ask whether <p> is entailed by the knowledge base"
    putStrLn "  clear       Remove all propositions from the knowledge base"
    putStrLn "  show        Display the current state of the knowledge base"
    putStrLn "  help        Show this help file"
    putStrLn "  quit        Exit the PLTP"
