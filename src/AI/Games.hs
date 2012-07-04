{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module AI.Games where

import Data.Map (Map, (!))

import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Random as R

import AI.Util.Util
import AI.Util.Graph (Graph)

import qualified AI.Util.Graph as G

-- |The type used to represent utilities
type Utility = Double

-- |Type used to distinguish between players
data Player = Max | Min deriving (Eq,Show)

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

    -- | Return a list of legal (move, state) pairs
    successors :: g s a -> s -> [(a,s)]
    successors game s = [ (a, makeMove game a s) | a <- legalMoves game s ]

-----------------------
-- Search Algorithms --
-----------------------

-- | Given a state in a game, calculate the best move by searching forward all
--   the way to the terminal states.
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

-- | Search the game tree to determine the best action, using alpha-beta
--   pruning. This version searches all the way to the leaves.
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
alphaBetaSearch' :: (Game g s a) => Int -> g s a -> s -> a
alphaBetaSearch' lim game state = alphaBetaSearch game cutoffFn evalFn state
    where
        cutoffFn state depth = depth > lim
        evalFn = utility game

------------------
-- Game Players --
------------------

-- |Type synonym for a player - a function that takes a game and a state of
--  that game, and returns an action. The result type is IO a to allow for
--  reading moves from stdin or a file.
type GamePlayer g s a = g s a -> s -> IO a

-- |A human player - reads moves from stdin.
queryPlayer :: (Game g s a, Show s, Read a) => g s a -> s -> IO a
queryPlayer g s = putStr "Your move: " >> readLn

-- |A player that uses the minimax algorithm to make its move.
minimaxPlayer :: Game g s a => g s a -> s -> IO a
minimaxPlayer g s = return (minimaxDecision g s)

-- |A player that uses full alpha/beta search to make its move.
alphaBetaFullSearchPlayer :: Game g s a => g s a -> s -> IO a
alphaBetaFullSearchPlayer g s = return (alphaBetaFullSearch g s)

-- |A player that uses alpha/beta search with a cutoff.
alphaBetaPlayer :: Game g s a => Int -> g s a -> s -> IO a
alphaBetaPlayer n g s = return (alphaBetaSearch' n g s)

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

--------------------
-- Game Instances --
--------------------

-----------------------------
-- Example game (see Fig 5.2)

data GameExample s a = GameExample deriving (Show)

g :: GameExample String Int
g = GameExample

instance Game GameExample String Int where
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

-------------------------------
-- Tic Tac Toe on a h x v board

data TicTacToe s a = TTT { hT :: Int, vT :: Int, kT :: Int } deriving (Show)

type TTMove = (Int,Int)
type TTBoard = Map TTMove TTCounter
data TTCounter = O | X deriving (Eq,Show)

data TTState = TTS
    { boardTT :: TTBoard
    , toMoveTT :: TTCounter
    , utilityTT :: Utility
    , limsTT :: (Int,Int,Int) }

instance Show TTState where
    show s = concat $ L.intersperse row $
                map ((++"\n") . L.intersperse '|') (toChars s)
        where
            (h,_,_) = limsTT s
            row = (concat $ replicate (h-1) "-+") ++ "-\n"

toChars :: TTState -> [[Char]]
toChars (TTS board _ _ (h,v,_)) = reverse $ map (map f) board'
    where
        board' = [ [ M.lookup (i,j) board | i <- [0..h-1] ] | j <- [0..v-1] ]
        f (Just O) = 'O'
        f (Just X) = 'X'
        f Nothing  = ' '

ticTacToe :: TicTacToe TTState TTMove
ticTacToe = TTT 3 3 3

instance Game TicTacToe TTState TTMove where
    initial (TTT h v k) = TTS M.empty O 0 (h,v,k)

    toMove _ s = if toMoveTT s == O then Max else Min

    legalMoves (TTT h v _) (TTS board _ _ _) =
        [ (i,j) | i <- [0..h-1], j <- [0..v-1], M.notMember (i,j) board ]

    makeMove g move (TTS board p _ n) =
        let u = computeUtility g board move p
            other O = X
            other X = O
        in TTS (M.insert move p board) (other p) u n

    utility _ s p = let u = utilityTT s in if p == Max then u else -u

    terminalTest g s = utilityTT s /= 0 || null (legalMoves g s)

computeUtility :: TicTacToe TTState TTMove -> TTBoard -> TTMove -> TTCounter -> Utility
computeUtility (TTT _ _ k) board move player = 
    if f (0,1) || f (1,0)  || f (1,1) || f (1,-1)
        then if player == O then 1 else -1
        else 0
        where f x = kInARow k board move player x

kInARow :: Int -> TTBoard -> TTMove -> TTCounter -> (Int,Int) -> Bool
kInARow k board (x,y) p (dx,dy) = n1 + n2 - 1 >= k
    where
        board' = M.insert (x,y) p board
        fw = map (`M.lookup` board') ( zip [x,x+dx..] [y,y+dy..] )
        bk = map (`M.lookup` board') ( zip [x,x-dx..] [y,y-dy..] )
        n1 = length $ takeWhile (== Just p) fw
        n2 = length $ takeWhile (== Just p) bk

------------
-- Connect 4

data Connect4 s a = C (TicTacToe s a)

connect4 :: Connect4 TTState TTMove
connect4 = C (TTT 7 6 4)

instance Game Connect4 TTState TTMove where
    initial (C g) = initial g
    toMove (C g) s = toMove g s

    legalMoves (C g) s@(TTS board _ _ _) =
        [ (x,y) | (x,y) <- legalMoves g s, y == 0 || (x,y-1) `M.member` board ]

    makeMove (C g) move s = makeMove g move s
    utility (C g) s p = utility g s p
    terminalTest (C g) s = terminalTest g s
