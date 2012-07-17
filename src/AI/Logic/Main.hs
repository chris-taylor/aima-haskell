module AI.Logic.Main where

import Control.Monad
import Control.Monad.State

import qualified Data.List as L

import AI.Logic.Parser
import AI.Logic.Propositional
import AI.Util.Util

----------------------
-- Interactive Code --
----------------------

run :: IO ()
run = do
    putStrLn "Propositional Logic Theorem Prover"
    evalStateT loop []

loop :: StateT [Expr] IO ()
loop = do
    liftIO $ putStr "> "
    (cmd, rest) <- fmap (break (==' ')) (liftIO getLine)
    case cmd of
        "quit"  -> return ()
        "tell"  -> tell (strip rest) >> loop
        "ask"   -> ask (strip rest) >> loop
        "show"  -> get >>= (\ps -> liftIO $ showPremises ps) >> loop
        "help"  -> liftIO showHelp >> loop
        "clear" -> put [] >> loop
        ""      -> loop
        _       -> liftIO unknown >> loop

tell :: String -> StateT [Expr] IO ()
tell expr = case parseExpr expr of
    Nothing -> liftIO (putStrLn "***parseError")
    Just p  -> modify (associate p:)

ask :: String -> StateT [Expr] IO ()
ask expr = case parseExpr expr of
    Nothing -> liftIO (putStrLn "*** parse error") >> loop
    Just q  -> do
        ps <- get
        liftIO $ showPremises ps
        if plResolution (And ps) q
            then liftIO $ putStrLn $ "Entailed: " ++ show q
            else liftIO $ putStrLn $ "Not entailed: " ++ show q

showPremises :: [Expr] -> IO ()
showPremises ps = forM_ (enumerate ps) $
    \(n,p) -> putStrLn ("  " ++ show n ++ ". " ++ show p)

showHelp :: IO ()
showHelp = do
    putStrLn "  quit   - Exit the PLTP"
    putStrLn "  tell P - Store proposition P in the knowledge base"
    putStrLn "  ask P  - Ask whether P is entailed by the knowledge base"
    putStrLn "  clear  - Empty the knowledge base"
    putStrLn "  show   - Display the knowldge base"

unknown :: IO ()
unknown = putStrLn "***unknown command"
