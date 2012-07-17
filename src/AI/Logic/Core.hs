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

data LogicError  = ParseError | UnknownCommand | DefaultError
type ThrowsError = ErrorT LogicError IO
type Logic k     = StateT k ThrowsError

instance Error LogicError where noMsg = DefaultError

trapError :: Monad m => m a -> m ()
trapError c = c >> return ()

--------------------
-- Knowledge Base --
--------------------

-- |A class for knowledge bases, supporting operations 'tell' (to tell a new
--  fact), 'ask' (to query the knowledge base) and 'retract' (to un-tell a
--  fact). You should create instances of this specific to your application.
class (Show p, Eq p, Show q) => KB k p q r  where
    -- |Returns an empty knowledge base.
    empty :: k p q r

    -- |Store a new fact in the knowledge base.
    tell :: k p q r -> p -> k p q r

    -- |Query the knowledge base.
    ask :: k p q r -> q -> r

    -- |Retract a fact from the knowledge base.
    retract :: k p q r -> p -> k p q r

    -- |List the propositions stored in the knowledge base.
    axioms :: k p q r -> [p]

    -- |Ask if the knowledge base contains a particular fact.
    contains :: k p q r -> p -> Bool
    contains k p = p `elem` axioms k

-- |A simple knowledge base for propositional logic. We keep a list of known
--  propositions (the axioms) to be used as an inference base.
data PropKB p q r = PropKB [Expr]

-- |An instance of 'KB' for propositional knowledge bases. It uses the
--  'plResolution' algorithm to determine if a query is entailed by the KB.
instance KB PropKB Expr Expr Bool where
    empty                  = PropKB []
    tell     (PropKB ps) p = PropKB $ ps ++ conjuncts (toCnf p)
    retract  (PropKB ps) p = PropKB $ L.delete p ps
    ask      (PropKB ps) q = plResolution (And ps) q
    axioms   (PropKB ps)   = ps

-- |Concrete instance of a propositional logic knowledge base that will use
--  truth tables for inference.
data PropTTKB p q r = PropTTKB [Expr]

-- |The 'KB' instance for a knowledge base that uses truth tables for inference.
instance KB PropTTKB Expr Expr Bool where
    empty                    = PropTTKB []
    tell     (PropTTKB ps) p = PropTTKB $ ps ++ conjuncts (toCnf p)
    retract  (PropTTKB ps) p = PropTTKB $ L.delete p ps
    ask      (PropTTKB ps) q = ttEntails (And ps) q
    axioms   (PropTTKB ps)   = ps

----------------------
-- Interactive Code --
----------------------

run :: IO ()
run = do
    putStrLn "Propositional Logic Theorem Prover"
    trapError $ runErrorT $ evalStateT loop empty

loop :: Logic (PropKB Expr Expr Bool) ()
loop = do
    liftIO $ putStr "> "
    (cmd, rest) <- fmap (break (==' ')) (liftIO getLine)
    case cmd of
        "show"      -> get >>= (\kb -> liftIO $ showPremises kb) >> loop
        "help"      -> liftIO showHelp >> loop
        "tell"      -> parse rest >>= tellKB >> loop
        "ask"       -> parse rest >>= askKB >> loop
        "retract"   -> parse rest >>= retractKB >> loop
        "clear"     -> clear >> loop
        "quit"      -> return ()
        ""          -> loop
        _           -> liftIO unknown >> loop

parse :: String -> Logic (PropKB Expr Expr Bool) Expr
parse str = case parseExpr (strip str) of
    Nothing -> liftIO (putStrLn "***parse error") >> throwError ParseError
    Just p  -> return (associate p)

tellKB :: KB k p q r => p -> Logic (k p q r) ()
tellKB expr = modify (\kb -> tell kb expr)

askKB :: KB k p q Bool => q -> Logic (k p q Bool) ()
askKB expr = do
    kb <- get
    liftIO $ showPremises kb
    if ask kb expr
        then liftIO $ putStrLn $ "Entailed: " ++ show expr
        else liftIO $ putStrLn $ "Not entailed: " ++ show expr

retractKB :: KB k p q r => p -> Logic (k p q r) ()
retractKB expr = do
    kb <- get
    if kb `contains` expr
        then modify $ \kb -> retract kb expr
        else liftIO $ putStrLn $ "***expression " ++ show expr ++ " not in KB"

clear :: KB k p q r => Logic (k p q r) ()
clear = put empty

showPremises :: KB k p q r => k p q r -> IO ()
showPremises kb = forM_ (enumerate $ axioms kb) $
    \(n,p) -> putStrLn ("  " ++ show n ++ ". " ++ show p)

unknown :: IO ()
unknown = putStrLn "***unknown command"

showHelp :: IO ()
showHelp = do
    putStrLn "  tell <p>    Store proposition <p> in the knowledge base"
    putStrLn "  retract <p> Remove proposition <p> from the knowledge base"
    putStrLn "  ask <p>     Ask whether <p> is entailed by the knowledge base"
    putStrLn "  clear       Remove all propositions from the knowledge base"
    putStrLn "  show        Display the current state of the knowledge base"
    putStrLn "  help        Show this help file"
    putStrLn "  quit        Exit the PLTP"
