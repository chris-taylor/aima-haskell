{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module AI.Games where

import Data.Map (Map, (!))

import qualified Data.Map as M
import qualified System.Random as R

import AI.Util.Util
import AI.Util.Graph (Graph)

import qualified AI.Util.Graph as G

-- | The type used to represent utilities
type Utility = Double

-- | Type used to distinguish between players
data Player = Max | Min deriving (Eq,Show)

-- | A game is similar to a problem, but it has a utility for each
--   state and a terminal test instead of a path cost and a goal
--   test. To create a game, make an instance of this class and implement
--   initial, toMove, legalMoves, makeMove, utility and terminalTest. You
--   may want to override successors for efficiency.
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

------------------
-- Game Players --
------------------

type GamePlayer g s a = g s a -> s -> IO a

queryPlayer :: (Game g s a, Show s, Read a) => g s a -> s -> IO a
queryPlayer g s = do
    print s
    a <- readLn
    return a

minimaxPlayer :: Game g s a => g s a -> s -> IO a
minimaxPlayer g s = return (minimaxDecision g s)

randomPlayer :: Game g s a => g s a -> s -> IO a
randomPlayer g s = randomChoiceIO (legalMoves g s)


playGame :: Game g s a => g s a -> GamePlayer g s a -> GamePlayer g s a -> IO Double
playGame game p1 p2 = go (initial game)
    where
        go s = if terminalTest game s
            then return (utility game s Max)
            else do
                a <- player game s
                go (makeMove game a s)           
            where
                player = if toMove game s == Max then p1 else p2

--------------------
-- Game Instances --
--------------------

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
