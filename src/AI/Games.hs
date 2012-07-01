{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module AI.Games where

import Data.Map (Map, (!))

import qualified Data.Map as M

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
minimaxDecision game state = action
    where
        (action, _) = argMax (successors game state) (minValue . snd)

        player = toMove game state

        minValue s = if terminalTest game s
            then utility game s player
            else minimum [ maxValue s' | (_,s') <- successors game s ]
        
        maxValue s = if terminalTest game s
            then utility game s player
            else maximum [ minValue s' | (_,s') <- successors game s ]

--------------------
-- Game Instances --
--------------------

--data Tree s a = Leaf Utility | Branch s [(a,Tree s a)]

data TreeGame s a = TG
    { initTG :: s
    , toMoveTG :: Map s Player
    , legalMovesTG :: Map s [a]
    , makeMoveTG :: Map s (Map a s)
    , utilTG :: Map s Double
    , terminalTG :: s -> Bool }

instance (Ord s, Ord a) => Game TreeGame s a where
    initial t = initTG t

    toMove t s = toMoveTG t ! s

    legalMoves t s = legalMovesTG t ! s

    makeMove t a s = (makeMoveTG t ! s) ! a

    utility t s p = let u = utilTG t ! s in if p == Max then u else -u

    terminalTest t s = terminalTG t s
