module AI.Logic.Main where

import Control.Monad
import Control.Monad.State

import qualified Data.List as L

import AI.Logic.Parser
import AI.Logic.Propositional

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
        "tell"  -> case parseExpr (dropWhile (==' ') rest) of
            Nothing -> liftIO (putStrLn "*** parse error") >> loop
            Just p  -> modify (p:) >> loop
        "ask"   -> case parseExpr (dropWhile (==' ') rest) of
            Nothing -> liftIO (putStrLn "*** parse error") >> loop
            Just q  -> do
                ps <- get
                liftIO $ putStrLn "Premises:"
                (liftIO . print) `mapM_` ps
                if plResolution (And ps) q
                    then liftIO $ putStrLn $ "Entail " ++ show q
                    else liftIO $ putStrLn $ "Do not entail " ++ show q
                loop
        "show"  -> do
            ps <- get
            (liftIO . print) `mapM_` ps
            loop
        "help"  -> do
            liftIO $ putStrLn "  quit   - Exit the PLTP"
            liftIO $ putStrLn "  tell P - Store proposition P in the knowledge base"
            liftIO $ putStrLn "  ask P  - Ask whether P is entailed by the knowledge base"
            liftIO $ putStrLn "  clear  - Empty the knowledge base"
            liftIO $ putStrLn "  show   - Display the knowldge base"
            loop
        "clear" -> put [] >> loop
        ""      -> loop
        _       -> (liftIO $ putStrLn "*** unknown command") >> loop
