{-# LANGUAGE MultiParamTypeClasses #-}

module AI.Games where

-- | The type used to represent utilities
type Utility = Double

-- | Type used to distinguish between players
data Player = PlayerA | PlayerB

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

    -- | Return the value of this state to this player.
    utility :: g s a -> s -> Player -> Utility

    -- | Return True if this is a final state for the game.
    terminalTest :: g s a -> s -> Bool

    -- | Return a list of legal (move, state) pairs
    successors :: g s a -> s -> [(a,s)]
    successors game s = [ (m, makeMove game m s) | m <- legalMoves game s ]