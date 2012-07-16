{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module AI.Search.Adversarial where

import Control.DeepSeq
import Control.Monad
import Data.IORef
import System.IO.Unsafe

import AI.Util.Util

-- |The type used to represent utilities
type Utility = Double

-- |Type used to distinguish between players
data Player = Max | Min deriving (Eq,Show)

-- |Return the opponent of the this player.
opponent :: Player -> Player
opponent Max = Min
opponent Min = Max

-- |A game is similar to a problem, but it has a utility for each
--  state and a terminal test instead of a path cost and a goal
--  test. To create a game, make an instance of this class and implement
--  initial, toMove, legalMoves, makeMove, utility and terminalTest. You
--  may want to override successors for efficiency.
class Game g s a where
    -- | The initial state of the game.
    initial :: g s a -> s

    -- | Return the player whose move it is in this state
    toMove :: g s a -> s -> Player

    -- | Return a list of all the allowable moves at this point.
    legalMoves :: g s a -> s -> [a]

    -- | Return the state that results from making a move in this state.
    makeMove :: g s a -> a -> s -> s

    -- | Return the value of this terminal state to this player.
    utility :: g s a -> s -> Player -> Utility

    -- | Return True if this is a final state for the game.
    terminalTest :: g s a -> s -> Bool

    -- | Sort a list of moves from best to worst. The default implementation is
    --   to do no sorting, but you may want to override this as it can
    --   significantly improve the performance of alpha/beta search.
    sortMoves :: g s a -> [a] -> [a]
    sortMoves _ = id

    -- | You may want to define a heuristic function for the game, which
    --   evaluates how good a position is.
    heuristic :: g s a -> s -> Player -> Utility
    heuristic g s p = if terminalTest g s then utility g s p else 0

    -- | Return a list of legal (move, state) pairs
    successors :: g s a -> s -> [(a,s)]
    successors game s = [ (a, makeMove game a s) | a <- moves ]
        where
            moves = sortMoves game (legalMoves game s)

-----------------------
-- Search Algorithms --
-----------------------

-- |Type synonym for a player - a function that takes a game and a state of
--  the game, and returns an action.
type GamePlayer g s a = g s a -> s -> a

-- |Given a state in a game, calculate the best move by searching forward all
--  the way to the terminal states.
minimaxSearch :: (Game g s a) => GamePlayer g s a
minimaxSearch game state = a
    where
        player = toMove game state
        succs  = successors game state
        (a,_)  = argMax succs (minValue . snd)

        minValue s = if terminalTest game s
            then utility game s player
            else minimum [ maxValue s' | (_,s') <- successors game s ]

        maxValue s = if terminalTest game s
            then utility game s player
            else maximum [ minValue s' | (_,s') <- successors game s ]

-- |Minimax search with a cutoff. When the cutoff is reached we evaluate the
--  state with a heuristic function instead of looking at its utility.
minimaxCutoff :: (Game g s a) =>
                 (s -> Int -> Bool)         -- ^ Cutoff function
              -> (s -> Player -> Utility)   -- ^ Heuristic function
              -> GamePlayer g s a
minimaxCutoff cutoff heuristic game state = a
    where
        player = toMove game state
        succs  = successors game state
        (a,_)  = argMax succs (minValue 0 . snd)

        minValue depth state
            | terminalTest game state = utility game state player
            | cutoff state depth      = heuristic state player
            | otherwise               =
                minimum [ maxValue (1+depth) s | (_,s) <- successors game state ]

        maxValue depth state
            | terminalTest game state = utility game state player
            | cutoff state depth      = heuristic state player
            | otherwise               = 
                maximum [ minValue (1+depth) s | (_,s) <- successors game state ]

-- |Default implementation of minimaxCutoff, that uses the default heuristic
--  function from the game and cuts off the search at a depth limit.
minimaxCutoff' :: (Game g s a) => Int -> GamePlayer g s a
minimaxCutoff' lim game state = minimaxCutoff cutoff evalFn game state
    where
        cutoff state depth = depth == lim
        evalFn = heuristic game

-- |Search the game tree to determine the best action, using alpha-beta
--  pruning. This version searches all the way to the leaves.
alphaBetaSearch :: (Game g s a) => GamePlayer g s a
alphaBetaSearch game state = a
    where
        player = toMove game state
        succs  = successors game state
        (a,_)  = argMax succs (minValue negInf posInf . snd)

        minValue alpha beta state = if terminalTest game state
            then utility game state player
            else f posInf beta (map snd $ successors game state)
            where
                f v beta []     = v
                f v beta (s:ss) = if v <= alpha
                    then v
                    else f v' (min beta v') ss
                    where
                        v' = min v (maxValue alpha beta s)

        maxValue alpha beta state = if terminalTest game state
            then utility game state player
            else g negInf alpha (map snd $ successors game state)
            where
                g v alpha []     = v
                g v alpha (s:ss) = if v >= beta
                    then v
                    else g v' (max alpha v')  ss
                    where
                        v' = max v (minValue alpha beta s)

-- |Search the game tree to determine the best action using alpha-beta pruning.
--  This version cuts off the search and uses an evaluation function
alphaBetaCutoff :: (Game g s a) =>
                   (s -> Int -> Bool)       -- ^ Cutoff test
                -> (s -> Player -> Utility) -- ^ Evaluation function
                -> GamePlayer g s a
alphaBetaCutoff cutoffTest evalFn game state = a
    where
        player = toMove game state
        succs  = successors game state
        (a,_)  = argMax succs (minValue negInf posInf 0 . snd)

        minValue alpha beta depth state
            | terminalTest game state = utility game state player
            | cutoffTest state depth  = evalFn state player
            | otherwise               = 
                f posInf beta (map snd $ successors game state)
                where
                    f v beta []     = v
                    f v beta (s:ss) = if v <= alpha
                        then v
                        else f v' (min beta v') ss
                        where
                            v' = min v (maxValue alpha beta (1+depth) s)

        maxValue alpha beta depth state
            | terminalTest game state = utility game state player
            | cutoffTest state depth  = evalFn state player
            | otherwise = 
                g negInf alpha (map snd $ successors game state)
                where
                    g v alpha []     = v
                    g v alpha (s:ss) = if v >= beta
                        then v
                        else g v' (max alpha v')  ss
                        where
                            v' = max v (minValue alpha beta (1+depth) s)

-- |Version of alpha-beta search that cuts off the search at a depth limit,
--  and uses the utility of a state as its evaluation function.
alphaBetaCutoff' :: (Game g s a) => Int -> GamePlayer g s a
alphaBetaCutoff' lim game state = alphaBetaCutoff cutoffFn evalFn game state
    where
        cutoffFn state depth = depth == lim
        evalFn = heuristic game

----------------------
-- I/O Game Players --
----------------------

-- |Type synonym for an I/O player - a function that takes a game and a state
--  and returns an I/O action. The result type is IO a to allow for
--  reading moves from stdin or a file.
type GamePlayerIO g s a = g s a -> s -> IO a

-- |A player that chooses a move at random from all legal moves.
randomPlayer :: Game g s a => GamePlayerIO g s a
randomPlayer g s = randomChoiceIO (legalMoves g s)

-- |A human player - reads moves from stdin.
queryPlayer :: (Game g s a, Show s, Show a, Read a, Eq a) => GamePlayerIO g s a
queryPlayer g s = getMove
    where
        getMove = putStr "Your move: " >> getLine >>= interpret

        interpret command = case command of
            ""   -> getMove
            "?" -> showHelp >> getMove
            "m" -> print (legalMoves g s) >> getMove
            "q" -> error "Quitting game!"
            _    -> parse command

        parse command = case reads command of
            []      -> putStrLn "*** No parse" >> getMove
            (a,_):_ -> if a `elem` legalMoves g s
                then return a
                else putStrLn "*** Illegal move" >> getMove

        showHelp = do
            putStrLn "  ? -- display this help file"
            putStrLn "  m -- display list of legal moves"
            putStrLn "  q -- quit the game"

-- |A player that uses the minimax algorithm to make its move.
minimaxFullSearchPlayer :: Game g s a => GamePlayerIO g s a
minimaxFullSearchPlayer g s = return (minimaxSearch g s)

-- |A player that uses minimax with a cutoff to make its move.
minimaxPlayer :: Int -> Game g s a => GamePlayerIO g s a
minimaxPlayer n g s = return (minimaxCutoff' n g s)

-- |A player that uses full alpha/beta search to make its move.
alphaBetaFullSearchPlayer :: Game g s a => GamePlayerIO g s a
alphaBetaFullSearchPlayer g s = return (alphaBetaSearch g s)

-- |A player that uses alpha/beta search with a cutoff.
alphaBetaPlayer :: Game g s a => Int -> GamePlayerIO g s a
alphaBetaPlayer n g s = return (alphaBetaCutoff' n g s)

-- |A player that uses iterative deepening minimax search, looking as deep into
--  the search tree as possible in a fixed time (measured in seconds).
iterativeMinimaxPlayer :: (NFData a, Game g s a) => Double -> GamePlayerIO g s a
iterativeMinimaxPlayer t g s = liftM head (timeLimited lim result)
    where
        lim    = round (t * 1000000)
        result = iterativeMinimax g s

-- |A player that uses iterative deepening alpha/beta search, looking as deep
--  into the search tree as possible in the time limit (measured in seconds).
iterativeAlphaBetaPlayer :: (NFData a, Game g s a) => Double -> GamePlayerIO g s a
iterativeAlphaBetaPlayer t g s = liftM head (timeLimited lim result)
    where
        lim    = round (t * 1000000)
        result = iterativeAlphaBeta g s

-- |Repeatedly try depth-limited minimax search with an increasing depth limit.
--  This function returns a list of moves, with the nth element in the list
--  corresponding to minimax with a cutoff of n.
iterativeMinimax :: (NFData a, Game g s a) => g s a -> s -> [a]
iterativeMinimax game state =
    map (\d -> minimaxCutoff' d game state) [0..1000]

-- |Repeatedly try depth-limited alpha-beta search with an increasing depth
--  limit. This function returns a list of moves, each resulting from a deeper
--  search into the game tree.
iterativeAlphaBeta :: (NFData a, Game g s a) => g s a -> s -> [a]
iterativeAlphaBeta game state =
    map (\d -> alphaBetaCutoff' d game state) [0..1000]

----------------------------
-- Routines to Play Games --
----------------------------

-- |Play a game between two players, returning the result and the sequence
--  of moves made.
playGame :: (Game g s a) =>
            g s a
         -> GamePlayer g s a
         -> GamePlayer g s a
         -> (Utility, [a])
playGame game p1 p2 = go (initial game) []
    where
        go state moves = if terminalTest game state
            then (util, reverse moves)
            else go (makeMove game action state) (action:moves)
            where
                util   = utility game state Max
                player = toMove game state
                action = if player == Max
                            then p1 game state
                            else p2 game state

-- |Play a game between two players, printing out the states and moves made
--  on each turn. The moves form each player are wrapped in the IO monad, to
--  allow for time-limited moves and random and human players.
playGameIO :: (Game g s a, Show s, Show a) =>
            g s a                 -- ^ Game to play
         -> GamePlayerIO g s a    -- ^ Player 1
         -> GamePlayerIO g s a    -- ^ Player 2
         -> IO s                  -- ^ Final state
playGameIO game p1 p2 = go (initial game)
    where
        go state = if terminalTest game state
            then printResult state
            else playRound state

        printResult state = do
            putStrLn "Final state is:" >> print state
            putStrLn ("Final score is " ++ show util ++ " (" ++ result ++ ")")
            return state
            where
                util   = utility game state Max
                result = if util == 0 then "Draw" else if util > 0
                    then "Player 1 Wins"
                    else "Player 2 Wins"

        playRound state = do
            putStrLn "Current state is:" >> print state
            action <- getMove game state
            putStrLn (show player ++ " plays " ++ show action)
            go (makeMove game action state)
            where
                player  = toMove game state
                getMove = if player == Max then p1 else p2

---------------------
-- Game Statistics --
---------------------

-- |Wrapper for a game that adds semantics for collecting statistics as the 
--  game is played.
data GameIO g s a = GIO
    { gameIO    :: g s a
    , numTerm   :: IORef Int
    , numSucc   :: IORef Int
    , numStates :: IORef Int }

-- |Wrap a game up in the GameIO type.
mkGameIO :: g s a -> IO (GameIO g s a)
mkGameIO g = do
    i <- newIORef 0
    j <- newIORef 0
    k <- newIORef 0
    return (GIO g i j k)

-- |Make GameIO into an instance of Game.
instance (Game g s a) => Game (GameIO g) s a where
    initial      (GIO g _ _ _) = initial g
    toMove       (GIO g _ _ _) s = toMove g s
    legalMoves   (GIO g _ _ _) s = legalMoves g s
    makeMove     (GIO g _ _ v) a s = makeMove g a s
    utility      (GIO g _ _ _) s p = utility g s p
    sortMoves    (GIO g _ _ _) as  = sortMoves g as
    heuristic    (GIO g _ _ _) s p = heuristic g s p

    terminalTest (GIO g i _ _) s = unsafePerformIO $ do
        modifyIORef i (+1)
        return (terminalTest g s)

    successors (GIO g _ j k) s = unsafePerformIO $ do
        let succs = successors g s
        modifyIORef j (+1)
        modifyIORef k (+length succs)
        return succs

-- |Run an adverserial search algorithm to find the optimal move from a given
--  state, collecting statistics as we go.
runPlayerIO :: g s a
            -> s
            -> (GameIO g s a -> s -> a)
            -> IO (a,Int,Int,Int)
runPlayerIO game state player = do
    g@(GIO _ numSucc numExpand numMoves) <- mkGameIO game
    let action = player g state in action `seq` do
        i <- readIORef numSucc
        j <- readIORef numExpand
        k <- readIORef numMoves
        return (action, i, j, k)
