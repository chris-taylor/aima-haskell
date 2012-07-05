{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module AI.Games where

import Prelude hiding (catch)

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Map (Map, (!))
import Data.Maybe (catMaybes)
import System.IO.Unsafe

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified System.Random as R

import AI.Util.Util
import AI.Util.Graph (Graph)

import qualified AI.Util.Graph as G

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

-- |Given a state in a game, calculate the best move by searching forward all
--  the way to the terminal states.
minimaxDecision :: (Game g s a) => g s a -> s -> a
minimaxDecision game state = a
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

-- |Search the game tree to determine the best action, using alpha-beta
--  pruning. This version searches all the way to the leaves.
alphaBetaFullSearch :: (Game g s a) => g s a -> s -> a
alphaBetaFullSearch game state = a
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
alphaBetaSearch :: (Game g s a) =>
                   g s a                    -- ^ Game
                -> (s -> Int -> Bool)       -- ^ Cutoff test
                -> (s -> Player -> Utility) -- ^ Evaluation function
                -> s                        -- ^ Starting state
                -> a                        -- ^ Final move
alphaBetaSearch game cutoffTest evalFn state = a
    where
        player = toMove game state
        succs  = successors game state
        (a,_)  = argMax succs (minValue negInf posInf 0 . snd)

        minValue alpha beta depth state
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
alphaBetaSearch' :: (Game g s a) => Int -> g s a -> s -> a
alphaBetaSearch' lim game state = alphaBetaSearch game cutoffFn evalFn state
    where
        cutoffFn state depth = terminalTest game state || depth > lim
        evalFn = heuristic game

-- |Repeatedly try depth-limited alpha-beta search with an increasing depth
--  limit.
iterativeAlphaBeta :: (NFData a, Game g s a) => g s a -> s -> [a]
iterativeAlphaBeta game state = map (\d -> alphaBetaSearch' d game state) [0..]

------------------
-- Game Players --
------------------

-- |Type synonym for a player - a function that takes a game and a state of
--  that game, and returns an action. The result type is IO a to allow for
--  reading moves from stdin or a file.
type GamePlayer g s a = g s a -> s -> IO a

-- |A human player - reads moves from stdin.
queryPlayer :: (Game g s a, Show s, Show a, Read a, Eq a) => g s a -> s -> IO a
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
minimaxPlayer :: Game g s a => g s a -> s -> IO a
minimaxPlayer g s = return (minimaxDecision g s)

-- |A player that uses full alpha/beta search to make its move.
alphaBetaFullSearchPlayer :: Game g s a => g s a -> s -> IO a
alphaBetaFullSearchPlayer g s = return (alphaBetaFullSearch g s)

-- |A player that uses alpha/beta search with a cutoff.
alphaBetaPlayer :: Game g s a => Int -> g s a -> s -> IO a
alphaBetaPlayer n g s = return (alphaBetaSearch' n g s)

-- |A player that uses iterative deepening alpha/beta search, looking as deep
--  into the search tree as possible in the time limit (measured in seconds).
iterativeAlphaBetaPlayer :: (NFData a, Game g s a) => Double -> g s a -> s -> IO a
iterativeAlphaBetaPlayer t g s = liftM head (timeLimited lim result)
    where
        lim    = round (t * 1000000)
        result = iterativeAlphaBeta g s

-- |A player that chooses a move at random from all legal moves.
randomPlayer :: Game g s a => g s a -> s -> IO a
randomPlayer g s = randomChoiceIO (legalMoves g s)

-- |Play a game between two players, printing out the states and moves made
--  on each turn.
playGame :: (Game g s a, Show s, Show a) =>
            g s a               -- ^ Game to play
         -> GamePlayer g s a    -- ^ Player 1
         -> GamePlayer g s a    -- ^ Player 2
         -> IO Utility          -- ^ Result of the game
playGame game p1 p2 = go (initial game)
    where
        go state = if terminalTest game state
            then printResult state
            else playRound state

        printResult state = do
            putStrLn "Final state is:" >> print state
            putStrLn ("Final score is " ++ show util ++ " (" ++ result ++ ")")
            return util
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

-- |Play a game, collecting statistics as we go.
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

--------------------
-- Game Instances --
--------------------

----------------------------
-- Example Game (Fig 5.2) --
----------------------------

-- |Data type representing the example game.
data ExampleGame s a = ExampleGame deriving (Show)

-- |Instance of the example game.
exampleGame :: ExampleGame String Int
exampleGame = ExampleGame

-- |Definition of the example game in Fig 5.2 (mainly useful as an example of
--  how to create games).
instance Game ExampleGame String Int where
    initial g = "A"

    toMove g "A" = Max
    toMove g  _  = Min

    legalMoves _ s = case s `elem` ["A","B","C","D"] of
        True  -> [1,2,3]
        False -> []

    makeMove _ n "A" = ["B","C","D"] !! (n-1)
    makeMove _ n "B" = ["B1","B2","B3"] !! (n-1)
    makeMove _ n "C" = ["C1","C2","C3"] !! (n-1)
    makeMove _ n "D" = ["D1","D2","D3"] !! (n-1)

    utility _ s p = let u = util s in if p == Max then u else -u
        where
            util = listToFunction [ ("B1", 3), ("B2",12), ("B3", 8)
                                  , ("C1", 2), ("C2", 4), ("C3", 6)
                                  , ("D1",14), ("D2", 5), ("D3", 2) ]

    terminalTest t s = if s `elem` ["B1","B2","B3","C1","C2","C3","D1","D2","D3"]
        then True
        else False

----------------------------------
-- Tic Tac Toe on a H x V board --
----------------------------------

-- |Data type for K-in-a-row  tic tac toe, on a H x V board.
data TicTacToe s a = TTT { hT :: Int, vT :: Int, kT :: Int } deriving (Show)

-- |A move in tic tac toe is a pair of integers indicating the row and column,
--  indexed from zero.
type TTMove = (Int,Int)

-- |Each counter in tic-tac-toe is either an @O@ or an @X@.
data TTCounter = O | X deriving (Eq,Show)

-- |A tic tac toe board is a map from board positions to counters. Note that
--  @M.lookup (x,y) board@ will return @Nothing@ if square @(x,y)@ is empty. 
type TTBoard = Map TTMove TTCounter

-- |The state of a tic tac toe game is defined by the board. We also store the
--  player whose move is next, the utility of this state (which is only nonzero
--  if the state is terminal) and the size of the board, for convenience.
data TTState = TTS
    { boardTT :: TTBoard
    , toMoveTT :: TTCounter
    , utilityTT :: Utility
    , limsTT :: (Int,Int,Int) }

-- |This 'Game' instance defines the rules of tic tac toe. Note that whenever
--  a move is made, we compute the utility of the newly created state on the
--  fly. This avoids having to write an expensive function to decide if any
--  player has won for a specific board state. The game is over when either
--  a player has one, or there are no legal moves left to make.
instance Game TicTacToe TTState TTMove where
    initial (TTT h v k) = TTS M.empty O 0 (h,v,k)

    toMove _ s = if toMoveTT s == O then Max else Min

    legalMoves (TTT h v _) (TTS board _ _ _) =
        [ (i,j) | i <- [0..h-1], j <- [0..v-1], M.notMember (i,j) board ]

    makeMove g move s@(TTS board p _ n) =
        let u = computeUtility s move
        in TTS (M.insert move p board) (other p) u n

    utility _ s p = let u = utilityTT s in if p == Max then u else -u

    terminalTest g s = utilityTT s /= 0 || null (legalMoves g s)

    heuristic _ = heuristicTTT [1,-1,0,0]

-- |A 3x3 instance of tic tac toe.
ticTacToe :: TicTacToe TTState TTMove
ticTacToe = TTT 3 3 3

-- |A useful function that interchanges @O@s and @X@s.
other :: TTCounter -> TTCounter
other O = X
other X = O

-- |In our game, @Max@ always plays the @O@ counter and @Min@ plays @X@.
counter :: Player -> TTCounter
counter Max = O
counter Min = X

-- |Helper function that computes the utility of a state after a particular
--  move is played.
computeUtility :: TTState -> TTMove -> Utility
computeUtility s@(TTS _ player _ _) move = if kInARow s move player
    then if player == O then 1 else -1
    else 0

-- |Given the current state of the board, return @True@ if putting a counter
--  into a specific square would win the game.
kInARow :: TTState -> TTMove -> TTCounter -> Bool
kInARow state move player = f (1,0) || f (0,1) || f (1,1) || f (1,-1)
    where
        f = kInARow' state move player

-- |A helper function for 'kInARow'. Given a state of the board, does adding a
--  counter to a specific square given k-in-a-row in the specified direction?
kInARow' :: TTState -> TTMove -> TTCounter -> (Int,Int) -> Bool
kInARow' (TTS board _ _ (_,_,k)) (x,y) p (dx,dy) = n1 + n2 - 1 >= k
    where
        board' = M.insert (x,y) p board
        fw = map (`M.lookup` board') ( zip [x,x+dx..] [y,y+dy..] )
        bk = map (`M.lookup` board') ( zip [x,x-dx..] [y,y-dy..] )
        n1 = length $ takeWhile (== Just p) fw
        n2 = length $ takeWhile (== Just p) bk

-- |The Show instance for 'TTState' creates a human-readable representation of
--  the board.
instance Show TTState where
    show s = concat $ concat $ L.intersperse [row] $
                map ((++["\n"]) . L.intersperse "|") (toChars s)
        where
            (h,_,_) = limsTT s
            row = (concat $ replicate (h-1) "---+") ++ "---\n"

-- |A helper function for @Show TTState@ that converts each position on the
--  board to its @Char@ representation.
toChars :: TTState -> [[String]]
toChars (TTS board _ _ (h,v,_)) = reverse $ map (map f) board'
    where
        board' = [ [ M.lookup (i,j) board | i <- [0..h-1] ] | j <- [0..v-1] ]
        f (Just O) = " O "
        f (Just X) = " X "
        f Nothing  = "   "

---------------
-- Connect 4 --
---------------

-- |The 'Connect4' data type is a wrapper around 'TicTacToe', which allows us
--  to inherit most of its behaviour.
data Connect4 s a = C (TicTacToe s a)

-- |The standard Connect 4 board.
connect4 :: Connect4 TTState TTMove
connect4 = C (TTT 7 6 4)

-- |A Connect 4 game is identical to tic tac toe in most respects. It differs in
--  the set of legal moves, the fact that it sorts moves so that those closest
--  to the center are considered first, and the heuristic function.
instance Game Connect4 TTState TTMove where
    initial      (C g)        = initial g
    toMove       (C g) s      = toMove g s
    makeMove     (C g) move s = makeMove g move s
    utility      (C g) s p    = utility g s p
    terminalTest (C g) s      = terminalTest g s

    sortMoves (C (TTT h _ _)) as = L.sortBy (O.comparing f) as
        where f (_,y) = abs (y - h `div` 2)

    legalMoves (C g) s@(TTS board _ _ _) =
        [ (x,y) | (x,y) <- legalMoves g s, y == 0 || (x,y-1) `M.member` board ]

    heuristic _ = heuristicTTT [0.1,-0.1,1,-1]

------------------------
-- Compute heuristics --
------------------------

-- |A heuristic function for Tic Tac Toe. If the game is won or lost, a value of
--  positive or negative infinity is assigned. Otherwise the value is a weighted
--  combination of simpler heuristic functions.
heuristicTTT :: [Double] -> TTState -> Player -> Utility
heuristicTTT weights s p
    | u > 0 = posInf
    | u < 0 = negInf
    | otherwise = sum $ zipWith (*) weights [n1,n2,n3,n4]
    where
        u  = if p == Max then utilityTT s else negate (utilityTT s)
        n1 = fromIntegral $ numWinningLines p s
        n2 = fromIntegral $ numWinningLines (opponent p) s
        n3 = fromIntegral $ numThreats p s
        n4 = fromIntegral $ numThreats (opponent p) s

-- |Compute the number of winning lines heuristic for a particular player. A
--  winning line is defined to be a line of k squares, which contains at least
--  one of your counters, and none of the opponents counters.
numWinningLines :: Player -> TTState -> Int
numWinningLines p s = length $ filter (isWinningLine $ counter p) (allLines s)

-- |Compute the number of threats heuristic for a particular player. A threat
--  cell is a cell that would win the game if it was filled in, but is not
--  blockable on the next move (i.e. it is not on the bottom row and does not
--  have a counter directly beneath it).
numThreats :: Player -> TTState -> Int
numThreats p s@(TTS _ _ _ (h,v,_)) = length $ filter (isThreat s p) xs
    where
        xs = [ (i,j) | i <- [0..h-1], j <- [0..v-1] ]

-- |Return @True@ is a specic cell is a threat cell.
isThreat :: TTState -> Player -> (Int,Int) -> Bool
isThreat s@(TTS board _ _ _) p (x,y) =
    y /= 0 && (x,y-1) `M.notMember` board && kInARow s (x,y) (counter p)

-- |Return @True@ if a line of pieces is a winning line.
isWinningLine :: TTCounter -> [Maybe TTCounter] -> Bool
isWinningLine c xs = c `elem` ys && not (other c `elem` ys)
    where
        ys = catMaybes xs

-- |Return a list of all of the lines on the board.
allLines :: TTState -> [[Maybe TTCounter]]
allLines s = concat [ linesInDir s (1,0), linesInDir s (0,1)
                    , linesInDir s (1,1), linesInDir s (1,-1) ]

-- |Return all of the lines on the board in the specified direction.
linesInDir :: TTState -> (Int,Int) -> [[Maybe TTCounter]]
linesInDir s@(TTS board _ _ (h,v,k)) dir = 
    map (\p -> lineThrough s p dir) pts
    where
        pts = case dir of
            (1,0)  -> [ (x,y) | x <- [0..h-k], y <- [0..v-1] ]
            (0,1)  -> [ (x,y) | x <- [0..h-1], y <- [0..v-k] ]
            (1,1)  -> [ (x,y) | x <- [0..h-k], y <- [0..v-k] ]
            (1,-1) -> [ (x,y) | x <- [0..h-k], y <- [k-1..v-1] ]

-- |Return the line starting in cell (x,y) and continuing in direction (dx,dy)
lineThrough :: TTState -> (Int,Int) -> (Int,Int) -> [Maybe TTCounter]
lineThrough (TTS board _ _ (h,v,k)) (x,y) (dx,dy) = 
    take k $ map (`M.lookup` board) ( zip [x,x+dx..] [y,y+dy..] )
