module AI.Logic.Interactive where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import qualified Data.List as L

import AI.Logic.Core
import AI.Logic.Propositional
import qualified AI.Logic.FOL as F
import AI.Util.Util

-----------
-- Types --
-----------

type IOThrowsError = ErrorT LogicError IO
type Logic k       = StateT k IOThrowsError

-- |Run a computation of type `Logic k'. The computation represents a live
--  interaction with a knowledge base. We don't care about the result - we
--  just want to get the side effects from storing premises in the knowledge
--  base and querying it for new information.
runLogic :: Logic k a -> k -> IO ()
runLogic c s = ignoreResult $ runErrorT $ evalStateT c s

----------------------
-- Interactive Code --
----------------------

-- |Start an interaction with a propositional logic theorem prover that uses
--  a resolution algorithm to do inference.
runProp :: IO ()
runProp = do
    putStrLn "Propositional Logic Resolution Theorem Prover"
    runLogic loop (empty :: PropKB PLExpr Bool)

-- |Start an interaction with a propositional logic theorem prover that uses
--  truth tables to do inference.
runTruthTable :: IO ()
runTruthTable = do
    putStrLn "Propositional Logic Truth Table Theorem Prover"
    runLogic loop (empty :: TruthTableKB PLExpr Bool)

-- |Start an interaction with a theorem prover that uses Horn clause and forward
--  chaining to do inference.
runForwardChaining :: IO ()
runForwardChaining = do
    putStrLn "Proposition Logic Forward Chaining Theorem Prover"
    runLogic loop (empty :: DefClauseKB DefiniteClause Bool)

-- |Start an interaction with a first-order logic theorem prover that uses
--  forward chaining.
runFOL :: IO ()
runFOL = do
    putStrLn "First Order Logic Theorem Prover"
    runLogic loop (empty :: F.FCKB F.DefiniteClause F.Term)

-- |The input/output loop for a theorem prover. We repeatedly ask for input
--  from the user, and then dispatch on the result, until the user enters the
--  command @"quit"@.
loop :: KB k p t => Logic (k p t) ()
loop = untilM (== "quit") (liftIO readPrompt) (trapError . dispatch)

-- |Decide what to do with user input to a theorem prover.
dispatch :: KB k p t => String -> Logic (k p t) ()
dispatch str = case cmd of
    "show"      -> get >>= (\kb -> liftIO $ showPremises kb)
    "help"      -> liftIO showHelp
    "tell"      -> parse rest >>= tellKB
    "ask"       -> parse rest >>= askKB
    "satisfy"   -> parse rest >>= satisfyKB
    "retract"   -> parse rest >>= retractKB
    "clear"     -> clear
    ""          -> return ()
    _           -> liftIO unknown
    where
        (cmd,rest) = break (== ' ') str

-- |Parse an expression entered by the user to be passed into either `tellKB',
--  `askKB' or `retractKB'.
parse :: KB k p t => String -> Logic (k p t) p
parse str = case parseExpr (strip str) of
    Left _  -> liftIO (putStrLn "***parse error") >> throwError ParseError
    Right p -> return p

-- |Store a new premise in the knowledge base.
tellKB :: KB k p t => p -> Logic (k p t) ()
tellKB expr = modify (\kb -> tell kb expr)

-- |Query the knowledge base.
askKB :: KB k p t => p -> Logic (k p t) ()
askKB expr = do
    kb <- get
    liftIO $ showPremises kb
    if ask kb expr
        then liftIO $ putStrLn $ "Entailed: " ++ show expr
        else liftIO $ putStrLn $ "Not entailed: " ++ show expr

satisfyKB :: KB k p t => p -> Logic (k p t) ()
satisfyKB expr = do
    kb <- get
    liftIO $ showPremises kb
    case askVars kb expr of
        [] -> liftIO $ putStrLn $ "No assignments satisfy: " ++ show expr
        xs -> liftIO $ putStrLn $ "Assignments: " ++ show xs

-- |Remove a premise from the knowledge base.
retractKB :: KB k p t => p -> Logic (k p t) ()
retractKB expr = do
    kb <- get
    if kb `contains` expr
        then modify $ \kb -> retract kb expr
        else liftIO $ putStrLn $ "***expression " ++ show expr ++ " not in KB"

-- |Empty the knowledge base of all previously entered premises.
clear :: KB k p t => Logic (k p t) ()
clear = put empty

-- |Display a list of all premises currently stored in the knowledge base.
showPremises :: KB k p t => k p t -> IO ()
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

