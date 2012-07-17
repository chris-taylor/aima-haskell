module AI.Logic.Main where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import qualified Data.List as L

import AI.Logic.Parser
import AI.Logic.Propositional
import AI.Util.Util

----------------------
-- Interactive Code --
----------------------

data LogicError  = ParseError | UnknownCommand | DefaultError

instance Error LogicError where noMsg = DefaultError

type ThrowsError = ErrorT LogicError IO
type KB          = StateT [Expr] ThrowsError

run :: IO (Either LogicError ())
run = do
    putStrLn "Propositional Logic Theorem Prover"
    runErrorT $ evalStateT loop []

loop :: KB ()
loop = do
    liftIO $ putStr "> "
    (cmd, rest) <- fmap (break (==' ')) (liftIO getLine)
    case cmd of
        "show"      -> get >>= (\ps -> liftIO $ showPremises ps) >> loop
        "help"      -> liftIO showHelp >> loop
        "tell"      -> parse rest >>= tell >> loop
        "ask"       -> parse rest >>= ask >> loop
        "retract"   -> parse rest >>= retract >> loop
        "clear"     -> clear >> loop
        "quit"      -> return ()
        ""          -> loop
        _           -> unknown >> loop

parse :: String -> KB Expr
parse str = case parseExpr (strip str) of
    Nothing -> liftIO (putStrLn "***parse error") >> throwError ParseError
    Just p  -> return (associate p)

tell :: Expr -> KB ()
tell expr = modify (++[expr])

ask :: Expr -> KB ()
ask expr = do
    ps <- get
    liftIO $ showPremises ps
    if plResolution (And ps) expr
        then liftIO $ putStrLn $ "Entailed: " ++ show expr
        else liftIO $ putStrLn $ "Not entailed: " ++ show expr

retract :: Expr -> KB ()
retract expr = do
    ps <- get
    if expr `elem` ps
        then modify $ L.delete expr
        else liftIO $ putStrLn $ "***expression " ++ show expr ++ " not in KB"

clear :: KB ()
clear = put []

unknown :: KB ()
unknown = liftIO (putStrLn "***unknown command") >> throwError UnknownCommand

showPremises :: [Expr] -> IO ()
showPremises ps = forM_ (enumerate ps) $
    \(n,p) -> putStrLn ("  " ++ show n ++ ". " ++ show p)

showHelp :: IO ()
showHelp = do
    putStrLn "  tell P    Store proposition P in the knowledge base"
    putStrLn "  retract P Remove proposition P from the knowledge base"
    putStrLn "  ask P     Ask whether P is entailed by the knowledge base"
    putStrLn "  clear     Remove all propositions from the knowledge base"
    putStrLn "  show      Display the current state of the knowledge base"
    putStrLn "  help      Show this help file"
    putStrLn "  quit      Exit the PLTP"
