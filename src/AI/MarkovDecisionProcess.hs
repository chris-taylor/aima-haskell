{-# LANGUAGE MultiParamTypeClasses #-}

module AI.MarkovDecisionProcess where

import Data.Map (Map, (!))
import GHC.Float

import qualified Data.Map as Map

import AI.Util.ProbDist
import AI.Util.Util

-- |Type for a utility function (a mapping from states to utilities)
type Utility s = s -> Double

-- |Type for a policy (a mapping from states to actions)
type Policy s a = s -> a

-- |Class for a Markov Decision Process. An MDP is defined by an initial state,
--  a transition model and a reward function. We may also specify a discount
--  factor (often called gamma). The transition model is represented somewhat
--  differently to the text. Instead of T(s, a, s') being a probability for each
--  state/action/state triplet, we instead have T(s,a) return a probability
--  distribution over states. We also keep track of the possible states,
--  terminal states and actions for each state.
class Ord s => MDP m s a where
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
    actions :: m s a -> s -> [a]
    actions m s = if s `elem` terminalStates m then [] else actionList m

--------------------
-- MDP Algorithms --
--------------------

-- |Given an MDP and a utility function, determine the best policy.
bestPolicy :: MDP m s a => m s a -> Utility s -> Policy s a
bestPolicy mdp u = listToFunction [ (s, bestAction s) | s <- stateList mdp ]
    where
        bestAction s = argMax (actions mdp s) (expectedUtility mdp u s)

-- |The expected utility of taking action @a@ in state @s@, according to the
--  decision process and a particular utility function.
expectedUtility :: MDP m s a => m s a -> Utility s -> s -> a -> Double
expectedUtility mdp u s a =
    float2Double $ expectation $ fmap u $ transition mdp s a

-- |Solve a Markov Decision Process using value iteration.
valueIteration :: MDP m s a =>
                  m s a     -- ^ Problem to be solved
               -> Double    -- ^ Tolerance: determines when to terminate
               -> Utility s -- ^ The final utility function
valueIteration mdp epsilon = go (const 0.0)
    where
        go u = if delta < epsilon * (1 - gamma) / gamma then u1 else go u1
            where
                delta = maximum [ abs (u1 s - u s) | s <- states ]
                u1    = listToFunction $ map (\s -> (s, f s)) states
                f s   = reward mdp s + gamma * maximum (g s)
                g s   = [ expectedUtility mdp u s a | a <- actions mdp s ]

        gamma    = discountFactor mdp
        states   = stateList mdp

-- |Solve a Markov Decision Process using policy iteration.
--policyIteration :: MDP m s a =>
--                   m s a
--                -> Policy s a

