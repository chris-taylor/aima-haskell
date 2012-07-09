{-# LANGUAGE MultiParamTypeClasses #-}

module AI.MarkovDecisionProcess where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import AI.Probability

-- |Class for a Markov Decision Process. An MDP is defined by an initial state,
--  a transition model and a reward function. We may also specify a discount
--  factor (often called gamma). The transition model is represented somewhat
--  differently to the text. Instead of T(s, a, s') being a probability for each
--  state/action/state triplet, we instead have T(s,a) return a probability
--  distribution over states. We also keep track of the possible states,
--  terminal states and actions for each state.
class MDP m s a where
    -- |Return the initial state for the problem.
    initial :: m s a -> s

    -- |Return a list of the possible actions for this MDP.
    actionList :: m s a -> [a]

    -- |Return a list of all states for this MDP.
    stateList :: m s a -> [s]

    -- |Return a list of the terminal states for this MDP.
    terminalStates :: m s a -> [s]

    -- |Return the reward associated with a particular state.
    reward :: m s a -> s -> Double

    -- |Return a probability distribution over states for a given (state,action)
    --  pair.
    transition :: m s a -> s -> a -> Dist s

    -- |Return the discount factor 'gamma' for this MDP.
    discountFactor :: m s a -> Double
    discountFactor _ = 0.9

    -- |Return the list of actions that can be performed in this state. By
    --  default this is a fixed list of actions, except for at terminal states.
    --  You can override this method if you need to specialize by state.
    actions :: Eq s => m s a -> s -> [a]
    actions m s = if s `elem` terminalStates m then [] else actionList m